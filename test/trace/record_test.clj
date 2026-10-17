(ns trace.record-test
  "trace.record against a throwaway trace repo and project under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.git :as g :refer [git]]
            [trace.read :as r]
            [trace.record :as rec]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "trace-test")))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))

(defn- fresh! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.clj")) "(ns a)\n\n(defn f [] 1)\n")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (sh proj "git" "worktree" "add" "-q" "wt" "-b" "side")
    (str proj)))

(defn- ev [event m] (rec/handle! event m))

(deftest one-session
  (let [proj (fresh!) a (str (fs/path proj "a.clj"))]
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "change f"})
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Read" "tool_input" {"file_path" a}
                  "tool_response" {"file" {"content" "(ns a) ..."}}})
      (spit a "(ns a)\n\n(defn f [] 2)\n")
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Edit"
                  "tool_input" {"file_path" a "old_string" "(defn f [] 1)" "new_string" "(defn f [] 2)"}
                  "tool_response" "ok"})
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"}
                  "tool_response" {"stdout" "a.clj\nwt"}})
      (ev "tool" {"session_id" "s1" "cwd" proj "hook_event_name" "PostToolUseFailure" "tool_name" "Bash"
                  "tool_input" {"command" "false"} "error" "Exit code 1"})
      (testing "a second session in a worktree of the same project gets the next prompt"
        (ev "prompt" {"session_id" "s2" "cwd" (str (fs/path proj "wt")) "prompt" "other"})
        (ev "tool" {"session_id" "s2" "cwd" (str (fs/path proj "wt")) "tool_name" "Bash"
                    "tool_input" {"command" "pwd"} "tool_response" {"stdout" "x"}})
        (ev "stop" {"session_id" "s2" "last_assistant_message" "done 2"}))
      (ev "stop" {"session_id" "s1" "last_assistant_message" "f is 2 now"})

      (let [tips (r/tips)
            d (r/prompt-data tips "proj" "00")
            [rd ed run bad] (:steps d)]
        (is (= ["proj"] (r/mains tips)))
        (is (= ["00" "01"] (r/prompt-nos tips "proj")))
        (is (= "change f" (:prompt d)))
        (is (= "f is 2 now" (:reply d)))
        (is (= [:read :edit :run :run] (map :kind (:steps d))) "the baseline is not a step")
        (testing "a read's sha is the file as it was, stored in the trace repo"
          (is (= "(ns a)\n\n(defn f [] 1)\n" (:out (g/git* {} "cat-file" "-p" (:blob rd)))))
          (is (= "a.clj" (:read-file rd))))
        (testing "an edit's diff is just the edit"
          (is (= [["files/a.clj" 1 1]] (map (juxt :path :add :del) (:files ed)))))
        (is (not (:err run)))
        (is (:err bad) "a failure is marked")
        (testing "both prompts are merged into the project, nothing left open"
          (is (= 2 (count (str/split-lines (git "rev-list" "--merges" "proj")))))
          (is (empty? (r/live-branches)))
          (is (= (str (fs/path proj "wt")) (:cwd (r/prompt-data tips "proj" "01")))))))))

(deftest untraced
  (binding [g/*repo* (str (fs/path scratch "traces2"))]
    (rec/ensure-repo!)
    (is (nil? (rec/project-of "/etc")) "outside git and ~/projects: not traced")
    (is (nil? (rec/project-of g/*repo*)) "the trace repo itself: not traced")
    (ev "tool" {"session_id" "nobody" "cwd" "/tmp" "tool_name" "Bash" "tool_input" {"command" "ls"}})
    (is (= "" (git "for-each-ref" "refs/heads")) "a tool call without an open prompt records nothing")))

(deftest subagent-is-its-own-history
  (let [proj (fresh!)]
    (binding [g/*repo* (str (fs/path scratch "traces3"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "do it"})
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "echo main-1"}
                  "tool_response" {"stdout" "main-1"}})
      (ev "subagent-start" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "general-purpose"})
      (ev "tool" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "general-purpose"
                  "tool_name" "Bash" "tool_input" {"command" "echo agent-1"} "tool_response" {"stdout" "agent-1"}})
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "echo main-2"}
                  "tool_response" {"stdout" "main-2"}})
      (ev "stop" {"session_id" "s1" "last_assistant_message" "parent done"})
      (testing "the agent keeps recording after its parent stopped, on its own branch"
        (ev "tool" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "general-purpose"
                    "tool_name" "Bash" "tool_input" {"command" "echo agent-2"} "tool_response" {"stdout" "agent-2"}})
        (ev "subagent-stop" {"session_id" "s1" "agent_id" "ab12" "last_assistant_message" "agent done"}))
      (let [tips (r/tips)
            parent (r/prompt-data tips "proj" "00")
            agent (r/prompt-data tips "proj" "00-aab12")]
        (testing "the parent's prompt has only the parent's calls"
          (is (= ["echo main-1" "echo main-2"] (map #(get-in % [:call :code]) (:steps parent)))))
        (testing "the agent's branch has the agent's calls and its reply"
          (is (= ["echo agent-1" "echo agent-2"] (map #(get-in % [:call :code]) (:steps agent))))
          (is (= "agent done" (:reply agent))))
        (testing "the agent branch is a side line: cut from the parent's tip at the start, not merged"
          (is (= 1 (count (str/split-lines (git "rev-list" "--merges" "proj")))))
          (is (not= (get tips "proj-p00") (get tips "proj-p00-aab12")))
          (is (= ["00"] (r/prompt-nos tips "proj"))))
        (testing "every event keeps its raw payload as a note"
          (let [c (get tips "proj-p00-aab12")]
            (is (str/includes? (git "notes" "--ref=event" "show" c) "agent done"))))
        (is (empty? (r/live-branches)))))))
