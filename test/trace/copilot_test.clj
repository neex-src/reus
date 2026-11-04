(ns trace.copilot-test
  "trace.copilot: the payloads of one real Copilot CLI 1.0.105 session (test/copilot/payloads.jsonl, with its
  events.jsonl; created in copilot-lab, Haiku, one failed edit, a shell create, an edit, ls) run through the
  hook process into a throwaway trace repo under <reus>/.scratch/copilot-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [trace.copilot :as cop]
            [trace.files :as files]
            [trace.git :as g :refer [git]]
            [trace.health :as health]))

(def code (str (fs/parent (System/getProperty "babashka.config"))))
(def scratch (str (fs/path code ".scratch" "copilot-test")))
(def sid "59f32c65-5c45-4ff1-b0d7-2504f1ce8b60")

(use-fixtures :each (fn [t] (binding [health/*claude-dir* (str (fs/path scratch "no-claude")) health/*seen-dir* (str (fs/path scratch "seen"))] (t))))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))

(deftest tool-results
  (is (= "hi" (cop/tool-response {"result_type" "success" "text_result_for_llm" "hi\n<shellId: 0 completed with exit code 0>"})))
  (is (= "Exit code 2\nboom" (cop/tool-response {"result_type" "success" "text_result_for_llm" "boom\n<shellId: 3 completed with exit code 2>"})))
  (is (= {"text" "nope" "is_error" true} (cop/tool-response {"result_type" "failure" "text_result_for_llm" "nope"})))
  (is (= {"file_path" "/a" "old_string" "x" "new_string" "y" "command" "ls"}
         (cop/tool-input {"path" "/a" "old_str" "x" "new_str" "y" "command" "ls"}))))

(deftest events-the-adapter-ignores
  (is (nil? (cop/adapt {"hook_event_name" "SessionStart" "session_id" "s" "cwd" "/"})))
  (is (nil? (cop/adapt {"hook_event_name" "PreToolUse" "session_id" "s" "cwd" "/"}))))

(deftest a-real-copilot-session-becomes-a-trace
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj")) repo (str (fs/path scratch "traces"))
        home (str (fs/path scratch "copilot-home"))
        events (str (fs/path home "session-state" sid "events.jsonl"))
        sub #(-> % (str/replace "@PROJ@" proj) (str/replace "@EVENTS@" events))
        payloads (map sub (str/split-lines (slurp (str (fs/path code "test" "copilot" "payloads.jsonl")))))
        run! (fn [line]
               (p/shell {:in line :out :string :err :string
                         :extra-env {"TRACE_REPO" repo "TRACE_SEEN" (str (fs/path scratch "seen")) "COPILOT_HOME" home}}
                        "bb" "--config" (str (fs/path code "bb.edn")) "-m" "trace.copilot"))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj ".gitkeep")) "")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (fs/create-dirs (fs/parent events))
    (spit events (sub (slurp (str (fs/path code "test" "copilot" "events.jsonl")))))
    (doseq [line payloads]
      (let [ev (get (json/parse-string line) "hook_event_name")
            tool (get (json/parse-string line) "tool_name")
            cmd (get-in (json/parse-string line) ["tool_input" "command"])]
        ;; what Copilot's tool had done by the time its hook fired
        (when (and (= "PostToolUse" ev) (= "Bash" tool) (str/starts-with? cmd "printf"))
          (spit (str (fs/path proj "hello.txt")) "hi\n"))
        (when (and (= "PostToolUse" ev) (= "Edit" tool)) (spit (str (fs/path proj "hello.txt")) "hello\n"))
        (run! line)))
    (loop [n 0]
      (when (and (seq (files/entries (files/spool-dir repo))) (< n 300)) (Thread/sleep 50) (recur (inc n))))
    (binding [g/*repo* repo]
      (let [subj (str/split-lines (git "log" "--format=%s" (str (fs/file-name proj) "/s/59f32c65")))]
        (is (= ["response 00" "00.4 Bash: ls && echo \"---\" && cat hello.txt" "00.3 Edit: hello.txt" "00.2 Bash: printf 'hi\\n' > hello.txt && cat hello.txt"
                nil "prompt 00: create hello.txt containing hi, then use the edit tool to change hi to hello, then run ls"]
               (assoc (vec (take 6 subj)) 4 nil)))
        (is (str/starts-with? (nth subj 4) "00.1 Edit: "))
        (testing "the failed edit carries its error, every call its model, the response its answer and tokens"
          (let [b (str (fs/file-name proj) "/s/59f32c65")]
            (is (str/includes? (git "log" "-1" "--format=%B" (str b "~4")) "Error: Unhandled error: Error: ENOENT"))
            (is (= "claude-haiku-5-5" (second (re-find #"(?m)^Model: (.+)$" (git "log" "-1" "--format=%B" (str b "~2"))))))
            (is (= "21412" (second (re-find #"(?m)^Tokens-In: (.+)$" (git "log" "-1" "--format=%B" b)))))
            (is (str/starts-with? (git "show" (str b ":.claude-trace/reply.md")) "Done. I created `hello.txt`"))))
        (testing "nothing Claude-shaped was needed: the original payload is kept in the event note"
          (is (str/includes? (git "notes" "--ref=event" "show" (str (fs/file-name proj) "/s/59f32c65~1")) "text_result_for_llm")))
        (is (= "hello" (git "show" (str (fs/file-name proj) "/s/59f32c65~1:hello.txt"))))
        (is (= "hi" (git "show" (str (fs/file-name proj) "/s/59f32c65~3:hello.txt"))))))))
