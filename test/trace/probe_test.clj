(ns trace.probe-test
  "trace.probe: the hook measures the transcript and keeps it as a note, against a throwaway
  trace repo and transcripts under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.agents :as agents]
            [trace.git :as g :refer [git]]
            [trace.probe :as probe]
            [trace.record :as rec]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "probe-test")))

(defn- line [m] (str (json/generate-string m) "\n"))
(defn- result [id] (line {"type" "user" "message" {"role" "user" "content" [{"type" "tool_result" "tool_use_id" id "content" "ok"}]}}))
(defn- say [t] (line {"type" "assistant" "message" {"role" "assistant" "content" [{"type" "text" "text" t}]}}))

(defn- fresh! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs proj)
    (p/shell {:dir (str proj) :out :string} "git" "init" "-q" "-b" "main")
    (str proj)))

(defn- probe-of [ref-branch]
  (edn/read-string (git "notes" "--ref=probe" "show" (git "rev-parse" ref-branch))))

(defn- ev [event m raw] (binding [rec/*probe?* true] (rec/handle! event m raw)))

(deftest found-at-once
  (let [proj (fresh!) tr (str (fs/path scratch "s1.jsonl"))]
    (spit tr (str (line {"type" "user" "message" {"content" "go"}}) (result "toolu_1")))
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "go" "transcript_path" tr} "{\"raw\":1}")
      (let [in {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_use_id" "toolu_1" "transcript_path" tr
                "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}}]
        (ev "tool" in (json/generate-string in))
        (let [pr (probe-of "proj-p00")]
          (is (= {:event "tool" :transcript tr :found? true} (select-keys pr [:event :transcript :found?])))
          (is (= (fs/size tr) (:bytes pr)))
          (is (< (:waited-ms pr) 100))
          (is (= (json/generate-string in) (git "notes" "--ref=event" "show" (git "rev-parse" "proj-p00")))
              "the event note stays the raw stdin"))
        (testing "the prompt was found, and as what"
          (is (= {:found? true :kind "user"} (select-keys (edn/read-string (git "notes" "--ref=probe" "show" (git "rev-parse" "proj-p00~1")))
                                                            [:found? :kind]))))))))

(deftest found-for-an-agent
  (let [proj (fresh!) parent (str (fs/path scratch "parent.jsonl"))
        pdir (str (fs/path scratch "projects"))
        af (fs/path pdir "p" "s1" "subagents" "agent-ab12.jsonl")]
    (fs/create-dirs (fs/parent af))
    (spit parent "")
    (spit (str af) (str (line {"type" "user" "message" {"content" "brief"}}) (result "toolu_a")))
    (with-redefs-fn {#'agents/projects-dir pdir}
      (fn []
        (binding [g/*repo* (str (fs/path scratch "traces2"))]
          (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "go" "transcript_path" parent} "{}")
          (ev "subagent-start" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "x" "transcript_path" parent} "{}")
          (is (true? (:found? (probe-of "proj-p00-aab12"))) "the agent's file exists")
          (ev "tool" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "tool_use_id" "toolu_a" "transcript_path" parent
                      "tool_name" "Bash" "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}} "{}")
          (let [pr (probe-of "proj-p00-aab12")]
            (is (true? (:found? pr)) "found in the agent's transcript, not the (empty) parent's")
            (is (= (str af) (:transcript pr))))
          (spit (str af) (say "agent done") :append true)
          (ev "subagent-stop" {"session_id" "s1" "agent_id" "ab12" "last_assistant_message" "agent done"} "{}")
          (is (true? (:found? (probe-of "proj-p00-aab12")))))))))

(deftest not-found-waits
  (let [proj (fresh!) tr (str (fs/path scratch "s3.jsonl"))
        tool #(ev "tool" {"session_id" "s3" "cwd" proj "tool_name" "Bash" "tool_use_id" % "transcript_path" tr
                          "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}} "{}")]
    (spit tr (str (say "earlier") (result "toolu_other")))
    (binding [g/*repo* (str (fs/path scratch "traces3"))]
      (ev "prompt" {"session_id" "s3" "cwd" proj "prompt" "go" "transcript_path" tr} "{}")
      (tool "toolu_missing")
      (let [pr (probe-of "proj-p00")]
        (is (false? (:found? pr)))
        (is (< (:waited-ms pr) 100) "a tool event is looked at once, no waiting")
        (is (nil? (:previous-found? pr)) "no previous tool call")
        (is (contains? pr :previous-found?))
        (is (nil? (:error pr))))
      (testing "the previous tool call is looked for at the next one"
        (tool "toolu_next")
        (is (false? (:previous-found? (probe-of "proj-p00"))) "toolu_missing is still not in the transcript")
        (spit tr (result "toolu_next") :append true)
        (tool "toolu_third")
        (is (true? (:previous-found? (probe-of "proj-p00")))))
      (testing "a reply that is still being written is found on a later look"
        (let [f (future (Thread/sleep 150) (spit tr (say "bye") :append true))]
          (ev "stop" {"session_id" "s3" "last_assistant_message" "bye" "transcript_path" tr} "{}")
          @f
          (let [pr (probe-of "proj-p00")]
            (is (true? (:found? pr)))
            (is (<= 100 (:waited-ms pr) 600)))))
      (testing "a transcript that is not there is a miss, not a failure"
        (ev "prompt" {"session_id" "s3" "cwd" proj "prompt" "again" "transcript_path" (str (fs/path scratch "none.jsonl"))} "{}")
        (ev "stop" {"session_id" "s3" "last_assistant_message" "bye" "transcript_path" (str (fs/path scratch "none.jsonl"))} "{}")
        (let [pr (probe-of "proj-p01")]
          (is (false? (:found? pr)))
          (is (nil? (:error pr))))))))

(deftest tail-only
  (let [tr (str (fs/path (fresh!) "big.jsonl"))]
    (spit tr (str (result "toolu_old") (apply str (repeat 3000 (say (apply str (repeat 200 "x"))))) (say "the reply")))
    (is (true? (:found? (probe/measure "stop" {"last_assistant_message" "the reply" "transcript_path" tr}))))
    (is (true? (:found? (probe/measure "tool" {"tool_use_id" "toolu_old" "transcript_path" tr})))
        "outside the first window, found by the widened last look")
    (is (nil? (:found? (probe/measure "stop" {"transcript_path" tr}))) "nothing to look for")))

(deftest untraced-is-not-probed
  (let [proj (fresh!) tr (str (fs/path scratch "u.jsonl"))
        t0 (System/currentTimeMillis)]
    (spit tr "")
    (binding [g/*repo* (str (fs/path scratch "traces4"))]
      (ev "prompt" {"session_id" "u" "cwd" "/etc" "prompt" "x" "transcript_path" tr} "{}")
      (ev "tool" {"session_id" "nobody" "cwd" proj "tool_name" "Bash" "tool_use_id" "t" "transcript_path" tr
                  "tool_input" {"command" "ls"}} "{}")
      (ev "stop" {"session_id" "nobody" "last_assistant_message" "x" "transcript_path" tr} "{}")
      (is (< (- (System/currentTimeMillis) t0) 400) "no 500 ms wait for events that record nothing")
      (is (= "" (git "for-each-ref" "refs/notes"))))))
