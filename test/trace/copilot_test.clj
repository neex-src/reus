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

(defn- replay!
  "The hook payloads of fixture dir (test/copilot/<name>) as Copilot's hook runs them: one trace.copilot
  process per payload, after `effect` did to the project what the tool did by then. Returns the trace repo's
  [repo project-name session-branch]."
  [name sid effect]
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj")) repo (str (fs/path scratch "traces"))
        home (str (fs/path scratch "copilot-home"))
        events (str (fs/path home "session-state" sid "events.jsonl"))
        dir (fs/path code "test" "copilot" name)
        sub #(-> % (str/replace "@PROJ@" proj) (str/replace "@EVENTS@" events))
        payloads (map sub (str/split-lines (slurp (str (fs/path dir "payloads.jsonl")))))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj ".gitkeep")) "")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (fs/create-dirs (fs/parent events))
    (spit events (sub (slurp (str (fs/path dir "events.jsonl")))))
    (doseq [line payloads]
      (effect proj (json/parse-string line))
      (p/shell {:in line :out :string :err :string
                :extra-env {"TRACE_REPO" repo "TRACE_SEEN" (str (fs/path scratch "seen")) "COPILOT_HOME" home}}
               "bb" "--config" (str (fs/path code "bb.edn")) "-m" "trace.copilot"))
    (loop [n 0]
      (when (and (seq (files/entries (files/spool-dir repo))) (< n 300)) (Thread/sleep 50) (recur (inc n))))
    [repo "proj" (str "proj/s/" (subs sid 0 8))]))

(defn- put! [proj content] (spit (str (fs/path proj "hello.txt")) content))
(defn- body [rev] (git "log" "-1" "--format=%B" rev))
(defn- trailer [rev k] (second (re-find (re-pattern (str "(?m)^" k ": (.+)$")) (body rev))))

(deftest a-copilot-session-with-a-failed-edit-becomes-a-trace
  (let [[repo _ b] (replay! "edit-fail" sid
                            (fn [proj {:strs [hook_event_name tool_name tool_input]}]
                              (when (= "PostToolUse" hook_event_name)
                                (case tool_name
                                  "Bash" (when (str/starts-with? (get tool_input "command") "printf") (put! proj "hi\n"))
                                  "Edit" (put! proj "hello\n")
                                  nil))))]
    (binding [g/*repo* repo]
      (let [subj (str/split-lines (git "log" "--format=%s" b))]
        (is (= ["response 00" "00.4 Bash: ls && echo \"---\" && cat hello.txt" "00.3 Edit: hello.txt"
                "00.2 Bash: printf 'hi\\n' > hello.txt && cat hello.txt"]
               (take 4 subj)))
        (is (str/starts-with? (nth subj 4) "00.1 Edit: "))
        (is (= "prompt 00: create hello.txt containing hi, then use the edit tool to change hi to hello, then run ls" (nth subj 5)))
        (is (= "hello" (git "show" (str b "~1:hello.txt"))))
        (is (= "hi" (git "show" (str b "~3:hello.txt"))))
        (testing "the failed edit carries its error, every call its model, the response its answer and tokens"
          (is (str/includes? (trailer (str b "~4") "Error") "ENOENT"))
          (is (= "claude-haiku-5-5" (trailer (str b "~2") "Model")))
          (is (= "21412" (trailer b "Tokens-In")))
          (is (str/starts-with? (git "show" (str b ":.claude-trace/reply.md")) "Done. I created `hello.txt`")))
        (testing "the original payload is kept in the event note"
          (is (str/includes? (git "notes" "--ref=event" "show" (str b "~1")) "text_result_for_llm")))
        (is (= "trace ok" (health/line)))))))

(deftest create-and-view
  (let [[repo _ b] (replay! "create-view" "c8b099e7-0a22-4595-b477-5ebcddc076ad"
                            (fn [proj {:strs [hook_event_name tool_name]}]
                              (when (= "PostToolUse" hook_event_name)
                                (case tool_name "Write" (put! proj "hi") "Edit" (put! proj "hi\nsecond line") nil))))]
    (binding [g/*repo* repo]
      (is (= ["response 00" "00.4 Bash: ls" "00.3 Edit: hello.txt" "00.2 Read: hello.txt" "00.1 Write: hello.txt"]
             (take 5 (str/split-lines (git "log" "--format=%s" b)))))
      (is (= "hi" (git "show" (str b "~3:hello.txt"))))
      (is (= "hi\nsecond line" (git "show" (str b "~1:hello.txt"))))
      (is (= "Write" (trailer (str b "~4") "Tool")))
      (is (= "hello.txt" (trailer (str b "~4") "File"))))))
