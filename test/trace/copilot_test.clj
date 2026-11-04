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
(use-fixtures :once (fn [t] (try (fs/delete-tree scratch) (catch Exception _)) (t)))

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
  ;; a dir of its own: a worker woken by an earlier replay may still be idling on that one's repo
  (let [base (str (fs/path scratch (str (System/nanoTime))))
        proj (str (fs/path base "proj")) repo (str (fs/path base "traces"))
        home (str (fs/path base "copilot-home"))
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
    (when (fs/exists? (fs/path dir "workspace.yaml")) ; as the IntelliJ plugin has it beside the log
      (spit (str (fs/path (fs/parent events) "workspace.yaml")) (sub (slurp (str (fs/path dir "workspace.yaml"))))))
    ;; the log as Copilot had written it when it ran hook k: up to that hook's hook.start
    (let [log (str/split-lines (sub (slurp (str (fs/path dir "events.jsonl")))))
          types (re-pattern (str "\"hookType\":\"(userPromptSubmitted|postToolUse|postToolUseFailure|agentStop|sessionEnd"
                                 (when (some #(str/includes? % "\"SessionStart\"") payloads) "|sessionStart") ")\""))
          starts (vec (keep-indexed #(when (and (str/starts-with? %2 "{\"type\":\"hook.start\"") ; only the hooks reus subscribes (the IDE logs more)
                                          (re-find types %2)) %1) log))]
      (is (= (count starts) (count payloads)))
      (doseq [[k line] (map-indexed vector payloads)]
        (spit events (str (str/join "\n" (take (inc (nth starts k)) log)) "\n"))
        (effect proj (json/parse-string line))
        (p/shell {:in line :out :string :err :string
                  :extra-env {"TRACE_REPO" repo "TRACE_SEEN" (str (fs/path base "seen")) "COPILOT_HOME" home}}
                 "bb" "--config" (str (fs/path code "bb.edn")) "-m" "trace.copilot"))
      (spit events (str (str/join "\n" log) "\n")))
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
          (is (= "8" (trailer b "Tokens-In")) "the uncached part; 13866 read and 7538 written are the cache")
          (is (= "13866" (trailer b "Cache-Read")))
          (testing "a call has the tokens of the model request that asked for it (the first call of a request only) and its time"
            (is (= "218" (trailer (str b "~3") "Tokens-Out")))
            (is (nil? (trailer (str b "~4") "Tokens-Out")))
            (is (< 0 (parse-long (trailer (str b "~3") "Duration-Ms")) 5000)))
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

(deftest the-reply-is-the-last-message-of-the-prompt
  (let [msg (fn [content tools] {"type" "assistant.message" "data" {"content" content "toolRequests" tools "model" "m"}})
        user {"type" "user.message" "data" {"content" "go"}}]
    (testing "a log that ends at a message with tool requests is not the end of the prompt"
      (is (false? (:final? (cop/turn-facts [user (msg "" [{"toolCallId" "a"}]) (msg "Now I'll edit" [{"toolCallId" "b"}])])))))
    (testing "the final message has no tool requests"
      (let [r (cop/turn-facts [user (msg "Now I'll edit" [{"toolCallId" "b"}]) (msg "All done." [])])]
        (is (true? (:final? r)))
        (is (= "All done." (:reply r)))))
    (testing "a prompt before it does not count"
      (is (false? (:final? (cop/turn-facts [(msg "old" []) user (msg "" [{"toolCallId" "a"}])])))))))

(deftest an-intellij-session-is-recorded-like-a-cli-one
  (testing "the plugin runs the same engine: its hooks (user-level, Pascal names) give the CLI payloads; the log says who ran it"
    (let [[repo _ b] (replay! "ide-create" "355f079f-34c9-4994-b0b5-7cb86052f1f5"
                              (fn [proj {:strs [hook_event_name tool_name]}]
                                (when (and (= "PostToolUse" hook_event_name) (= "Write" tool_name)) (put! proj "hi\n"))))]
      (binding [g/*repo* repo]
        (let [subj (str/split-lines (git "log" "--format=%s" b))]
          (is (= ["response 00" "00.1 Write: hello.txt"] [(first subj) (nth subj 2)]))
          (is (str/starts-with? (nth subj 1) "00.2 task_complete"))
          (is (= "claude-haiku-5-5" (trailer (str b "~1") "Model")))
          (is (str/includes? (git "notes" "--ref=event" "show" (str b "~2")) "\"client\":\"copilot-intellij\"")))))))

(deftest ide-details
  (testing "auto mode: the model is the one Copilot chose"
    (let [f (str (fs/path scratch "auto" "events.jsonl"))]
      (fs/create-dirs (fs/parent f))
      (spit f (str (json/generate-string {"type" "session.start" "data" {"selectedModel" "auto"}}) "\n"
                   (json/generate-string {"type" "session.auto_mode_resolved" "data" {"chosenModel" "gpt-6-luna"}}) "\n"))
      (is (= "gpt-6-luna" (cop/session-model f)))
      (is (nil? (cop/client-name f)))
      (spit (str (fs/path (fs/parent f) "workspace.yaml")) "id: x\nclient_name: copilot-intellij\n")
      (is (= "copilot-intellij" (cop/client-name f)))))
  (testing "a tool with one string argument (apply_patch)"
    (is (= {"input" "*** Begin Patch\n*** Add File: a/b.txt\n+hi\n*** End Patch\n" "file_path" "a/b.txt"}
           (cop/tool-input "*** Begin Patch\n*** Add File: a/b.txt\n+hi\n*** End Patch\n")))
    (is (= {} (cop/tool-input nil)))))

(deftest ide-model-choice
  (is (= ["auto" false] ((requiring-resolve 'trace.ide/choose-model) ["auto"] "auto" false)))
  (is (= ["claude-haiku-4.5" false] ((requiring-resolve 'trace.ide/choose-model) ["claude-haiku-4.5" "gpt-5"] "haiku" true)) "a listed model wins over BYOK")
  (is (= ["claude-haiku-5-5" true] ((requiring-resolve 'trace.ide/choose-model) ["auto"] "haiku" true)))
  (is (nil? ((requiring-resolve 'trace.ide/choose-model) ["auto"] "haiku" false))))
