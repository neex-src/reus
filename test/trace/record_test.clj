(ns trace.record-test
  "trace.record against a throwaway trace repo and project under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [cheshire.core]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.git :as g :refer [git]]
            [trace.health :as health]
            [trace.read :as r]
            [trace.record :as rec]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "trace-test")))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))
(defn- real [proj & args] (str/trim (:out (apply sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" args))))

(defn- fresh!
  "A project with one commit, and a worktree of it on branch side next to it."
  []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.clj")) "(ns a)\n\n(defn f [] 1)\n")
    (spit (str (fs/path proj ".gitignore")) "ignored/\n")
    (sh proj "git" "add" ".")
    (real proj "commit" "-q" "-m" "init")
    (real proj "worktree" "add" "-q" "../wt" "-b" "side")
    (str proj)))

(defn- ev [event m] (rec/handle! event m))
(defn- tree-files [rev] (set (str/split-lines (git "ls-tree" "-r" "--name-only" rev))))
(defn- trailer [rev k] (second (re-find (re-pattern (str "(?m)^" k ": (.+)$")) (git "log" "-1" "--format=%B" rev))))

(deftest one-session
  (let [proj (fresh!) a (str (fs/path proj "a.clj")) wt (str (fs/path scratch "wt"))
        head (real proj "rev-parse" "HEAD")]
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (fs/create-dirs (fs/path proj "ignored"))
      (spit (str (fs/path proj "ignored" "x")) "not in the trace")
      (spit (str (fs/path proj "new.txt")) "untracked, not ignored")
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
      (testing "a second session in a worktree of the same project has a branch of its own"
        (ev "prompt" {"session_id" "s2" "cwd" wt "prompt" "other"})
        (ev "tool" {"session_id" "s2" "cwd" wt "tool_name" "Bash"
                    "tool_input" {"command" "pwd"} "tool_response" {"stdout" "x"}})
        (ev "stop" {"session_id" "s2" "last_assistant_message" "done 2"}))
      (ev "stop" {"session_id" "s1" "last_assistant_message" "f is 2 now"})

      (let [tips (r/tips)
            d (r/prompt-data tips "proj" "s1@00")
            [rd ed run bad] (:steps d)
            b "proj/s/s1"]
        (is (= ["proj"] (r/mains tips)))
        (is (= ["s1@00" "s2@00"] (r/prompt-nos tips "proj")))
        (is (= "change f" (:prompt d)))
        (is (= "f is 2 now" (:reply d)))
        (is (= [:read :edit :run :run] (map :kind (:steps d))))
        (testing "a read is an empty diff of a whole tree, and its blob is the file as it was"
          (is (empty? (:files rd)))
          (is (= "(ns a)\n\n(defn f [] 1)\n" (:out (g/git* {} "cat-file" "-p" (:blob rd)))))
          (is (= "a.clj" (:read-file rd))))
        (testing "an edit's diff is just the edit"
          (is (= [["a.clj" 1 1]] (map (juxt :path :add :del) (:files ed)))))
        (testing "the tree is the worktree: tracked and untracked files, not the ignored ones, and .claude-trace"
          (let [fs* (tree-files (:sha ed))]
            (is (every? fs* ["a.clj" ".gitignore" "new.txt" ".claude-trace/prompt.md"]))
            (is (not-any? #(str/starts-with? % "ignored/") fs*))
            (is (not (fs* ".claude-trace/reply.md")) "the reply is written at the end")))
        (testing "the reply, after the last call"
          (is (= "f is 2 now" (git "show" (str b ":.claude-trace/reply.md")))))
        (testing "trailers and notes"
          (is (= "Edit" (trailer (:sha ed) "Tool")))
          (is (= "a.clj" (trailer (:sha ed) "File")))
          (is (= head (trailer (:sha ed) "Real-Head")))
          (is (:err bad) "a failure is marked")
          (is (= "1" (trailer (:sha bad) "Exit")))
          (is (= "a.clj\nwt" (git "cat-file" "-p" (:result run))))
          (is (some? (git "notes" "--ref=event" "show" (:sha ed)))))
        (testing "the worktree's session has the worktree's tree: its branch, not the main checkout's files"
          (is (not ((tree-files "proj/s/s2") "new.txt")))
          (is (= (real proj "rev-parse" "side") (trailer "proj/s/s2" "Real-Head"))))
        (testing "trace main mirrors the real main: one commit, same tree, the real message"
          (is (= (real proj "rev-parse" "HEAD^{tree}") (git "rev-parse" "proj/main^{tree}")))
          (is (= head (trailer "proj/main" "Real-Commit")))
          (is (str/starts-with? (git "log" "-1" "--format=%s" "proj/main") "init")))
        (testing "both sessions start from trace main, and nothing is left open"
          (is (= (git "rev-parse" "proj/main") (git "merge-base" "proj/main" b)))
          (is (empty? (r/live-branches))))
        (testing "the trace repo owns its objects: no alternates, complete once the project and its worktree are gone"
          (is (not (fs/exists? (fs/path g/*repo* ".git" "objects" "info" "alternates"))))
          (fs/delete-tree proj)
          (fs/delete-tree wt)
          (is (zero? (:exit (g/git* {} "fsck" "--no-dangling" "--connectivity-only")))))))))

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
            parent (r/prompt-data tips "proj" "s1@00")
            agent (r/prompt-data tips "proj" "s1-ab12@00-aab12")]
        (testing "the parent's prompt has only the parent's calls"
          (is (= ["echo main-1" "echo main-2"] (map #(get-in % [:call :code]) (:steps parent)))))
        (testing "the agent's branch has the agent's calls and its reply"
          (is (= ["echo agent-1" "echo agent-2"] (map #(get-in % [:call :code]) (:steps agent))))
          (is (= "agent done" (:reply agent)))
          (is (= "general-purpose" (:agent (first (:steps agent))))))
        (testing "cut from the parent's tip at the start; the parent has no link to it"
          (is (= (git "rev-parse" "proj/s/s1~2") (git "merge-base" "proj/s/s1" "proj/a/s1-ab12")))
          (is (= 1 (count (str/split (git "log" "-1" "--format=%P" "proj/s/s1") #" "))))
          (is (= ["s1@00" "s1-ab12@00-aab12"] (r/prompt-nos tips "proj"))))
        (testing "every event keeps its raw payload as a note"
          (is (str/includes? (git "notes" "--ref=event" "show" "proj/a/s1-ab12") "agent done")))
        (is (empty? (r/live-branches)))))))

(deftest agent-report-is-a-trailer
  (let [proj (fresh!)]
    (binding [g/*repo* (str (fs/path scratch "traces4"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "go"})
      (ev "subagent-start" {"session_id" "s1" "cwd" proj "agent_id" "late" "agent_type" "x"})
      (ev "subagent-start" {"session_id" "s1" "cwd" proj "agent_id" "early" "agent_type" "x"})
      (ev "subagent-stop" {"session_id" "s1" "agent_id" "early" "last_assistant_message" "e"})
      (testing "an agent that stops while the parent's prompt is open is reported in that prompt's response"
        (ev "stop" {"session_id" "s1" "last_assistant_message" "parent done"})
        (is (= (git "rev-parse" "proj/a/s1-early") (trailer "proj/s/s1" "Agent-Report"))))
      (testing "an agent that stops while the parent is idle is reported by the prompt its notification opens"
        (ev "subagent-stop" {"session_id" "s1" "agent_id" "late" "last_assistant_message" "l"})
        (ev "prompt" {"session_id" "s1" "cwd" proj
                      "prompt" "<task-notification>\n<task-id>late</task-id>\n</task-notification>"})
        (is (= (git "rev-parse" "proj/a/s1-late") (trailer "proj/s/s1" "Agent-Report")))
        (is (str/starts-with? (git "log" "-1" "--format=%s" "proj/s/s1") "prompt 01:")))
      (testing "no parent links anywhere"
        (is (= "" (str/join (filter #(> (count (str/split % #" ")) 1) (str/split-lines (git "log" "--format=%P" "proj/s/s1"))))))))))

(deftest merge-gets-the-branches-that-worked-in-it
  (let [proj (fresh!) wt (str (fs/path scratch "wt"))]
    (binding [g/*repo* (str (fs/path scratch "traces5"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "main work"})
      (ev "prompt" {"session_id" "s2" "cwd" wt "prompt" "feature work"})
      (spit (str (fs/path wt "feature.txt")) "feature\n")
      (real wt "add" ".")
      (real wt "commit" "-q" "-m" "feature: a file")
      (ev "tool" {"session_id" "s2" "cwd" wt "tool_name" "Bash" "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}})
      (ev "stop" {"session_id" "s2" "last_assistant_message" "ok"})
      (testing "a real commit on main is mirrored by the next hook, without extra parents"
        (spit (str (fs/path proj "m.txt")) "m\n")
        (real proj "add" ".")
        (real proj "commit" "-q" "-m" "plain commit")
        (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}})
        (is (= "plain commit" (git "log" "-1" "--format=%s" "proj/main")))
        (is (= 1 (count (str/split (git "log" "-1" "--format=%P" "proj/main") #" ")))))
      (testing "a real merge brings in the tip of the branch whose Real-Head is among the merged commits"
        (real proj "merge" "-q" "--no-ff" "side" "-m" "Merge side")
        (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}})
        (is (= "Merge side" (git "log" "-1" "--format=%s" "proj/main")))
        (let [ps (str/split (git "log" "-1" "--format=%P" "proj/main") #" ")]
          (is (= 2 (count ps)))
          (is (= (git "rev-parse" "proj/s/s2") (second ps))))
        (is (= (real proj "rev-parse" "HEAD^{tree}") (git "rev-parse" "proj/main^{tree}")))
        (testing "git log -p of a file through main shows the tool calls that led to it"
          (is (str/includes? (git "log" "--format=%s" "proj/main" "--" "feature.txt") "00.1 Bash: ls")))
        (is (str/includes? (git "log" "--format=%s" "proj/main") "prompt 00: feature work")
            "the session branch is part of main's history now")))))

(deftest prompts-keep-the-tree-and-agents-start-in-their-worktree
  (let [proj (fresh!) wt (str (fs/path scratch "wt")) tr (str (fs/path scratch "t.jsonl"))
        tree (fn [rev] (git "rev-parse" (str rev "^{tree}")))
        no-trace (fn [rev] (str/join "\n" (remove #(str/includes? % ".claude-trace") (str/split-lines (git "ls-tree" "-r" rev)))))]
    (spit tr (str (cheshire.core/generate-string
                   {"type" "assistant" "message" {"id" "m1" "content" [{"type" "tool_use" "id" "toolu_agent" "name" "Agent"
                                                                        "input" {"prompt" "read the file, tell me" "description" "d" "subagent_type" "Explore"}}]}})
                  "\n"))
    (binding [g/*repo* (str (fs/path scratch "traces6"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "go"})
      (spit (str (fs/path wt "feature.txt")) "one\n")
      (real wt "add" ".") (real wt "commit" "-q" "-m" "feat")
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Write" "tool_input" {"file_path" (str (fs/path wt "feature.txt"))}
                  "tool_response" "ok"})
      (let [tool-c (git "rev-parse" "proj/s/s1")
            wt-head (real wt "rev-parse" "HEAD")]
        (is (= wt-head (trailer tool-c "Real-Head")))
        (is (str/includes? (no-trace tool-c) "feature.txt") "a call in the worktree has the worktree's tree")
        (testing "a call that does not act in a worktree (an Agent call, a web fetch) keeps tree and Real-Head"
          (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Agent" "tool_input" {"prompt" "x"} "tool_response" "ok"})
          (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "WebFetch" "tool_input" {"url" "http://x"} "tool_response" "ok"})
          (is (= (no-trace tool-c) (no-trace "proj/s/s1")))
          (is (= wt-head (trailer "proj/s/s1" "Real-Head")))
          (testing "while a shell call in the main checkout switches to its tree"
            (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"} "tool_response" "ok"})
            (is (not (str/includes? (no-trace "proj/s/s1") "feature.txt")))
            (is (= (real proj "rev-parse" "HEAD") (trailer "proj/s/s1" "Real-Head")))
            (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Write" "tool_input" {"file_path" (str (fs/path wt "feature.txt"))}
                        "tool_response" "ok"})))
        (ev "stop" {"session_id" "s1" "cwd" proj "last_assistant_message" "ok"})
        (testing "the response, from the main checkout's cwd, keeps the tree of the previous commit"
          (is (= (no-trace tool-c) (no-trace "proj/s/s1")))
          (is (= wt-head (trailer "proj/s/s1" "Real-Head")) "and that worktree's HEAD")
          (is (str/includes? (git "show" "proj/s/s1:.claude-trace/reply.md") "ok")))
        (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "again"})
        (testing "so does the next prompt"
          (is (= (no-trace tool-c) (no-trace "proj/s/s1")))
          (is (= wt-head (trailer "proj/s/s1" "Real-Head")))
          (is (str/includes? (git "show" "proj/s/s1:.claude-trace/prompt.md") "again")))
        (testing "an agent started in the worktree: its brief is the parent's Agent call, its tree and Real-Head the worktree's"
          (ev "subagent-start" {"session_id" "s1" "cwd" wt "agent_id" "ab12" "agent_type" "Explore" "transcript_path" tr})
          (let [b "proj/a/s1-ab12"]
            (is (= "read the file, tell me" (git "show" (str b ":.claude-trace/prompt.md"))))
            (is (str/includes? (git "log" "-1" "--format=%s" b) "Explore: read the file"))
            (is (= wt-head (trailer b "Real-Head")))
            (is (str/includes? (no-trace b) "feature.txt"))))))))

(deftest vanished-root-is-never-fatal
  (let [proj (fresh!) wt (str (fs/path scratch "wt"))
        no-trace (fn [rev] (str/join "\n" (remove #(str/includes? % ".claude-trace") (str/split-lines (git "ls-tree" "-r" rev)))))
        bash (fn [cwd c] (ev "tool" {"session_id" "s1" "cwd" cwd "tool_name" "Bash" "tool_input" {"command" c} "tool_response" "ok"}))
        subjects (fn [] (str/split-lines (git "log" "--format=%s" "proj/s/s1")))]
    (binding [g/*repo* (str (fs/path scratch "traces7"))]
      (spit (str (fs/path wt "wt-only.txt")) "one\n")
      (ev "prompt" {"session_id" "s1" "cwd" wt "prompt" "work in the worktree"})
      (bash wt "ls")
      (let [before (no-trace "proj/s/s1")]
        (is (str/includes? before "wt-only.txt"))
        (real proj "worktree" "remove" "--force" wt)
        (testing "no root can be found: the events still commit and keep the previous tree"
          (bash wt "ls")
          (ev "stop" {"session_id" "s1" "cwd" wt "last_assistant_message" "gone"})
          (is (= before (no-trace "proj/s/s1")))
          (is (= ["response 00" "00.2 Bash: ls" "00.1 Bash: ls"] (take 3 (subjects))))
          (is (str/includes? (git "show" "proj/s/s1:.claude-trace/reply.md") "gone")))
        (testing "the next prompt from the main checkout finds the project again"
          (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "again"})
          (bash proj "ls")
          (is (= "01.1 Bash: ls" (first (subjects))))
          (is (not (str/includes? (no-trace "proj/s/s1") "wt-only.txt")))
          (is (= (real proj "rev-parse" "HEAD") (trailer "proj/s/s1" "Real-Head"))))
        (testing "a root that goes away mid-prompt: the event's cwd gives the fresh one"
          (real proj "worktree" "add" "-q" "../wt" "-b" "side2")
          (ev "prompt" {"session_id" "s1" "cwd" wt "prompt" "back in a worktree"})
          (spit (str (fs/path wt "second.txt")) "two\n")
          (real proj "worktree" "remove" "--force" wt)
          (bash proj "ls")
          (is (= "02.1 Bash: ls" (first (subjects))))
          (is (not (str/includes? (no-trace "proj/s/s1") "second.txt"))))))))

(deftest health-state
  (fresh!)
  (binding [g/*repo* (str (fs/path scratch "traces"))]
    (fs/create-dirs (fs/path g/*repo* ".git"))
    (is (= "trace ok" (health/line)))
    (health/fail! :worker "tool" {"session_id" "s1"} (ex-info "boom" {}))
    (let [since (second (re-find #"since (\S+):" (health/line)))]
      (is (str/ends-with? (health/line) ": boom"))
      (health/fail! :worker "stop" {"session_id" "s1"} (ex-info "bang" {}))
      (is (= (str "trace worker failing since " since ": bang") (health/line))))
    (health/ok! :worker)
    (is (= "trace ok" (health/line)))))

(deftest model-and-usage-from-the-payload
  (let [proj (fresh!)]
    (binding [g/*repo* (str (fs/path scratch "traces7"))]
      (testing "without a transcript, the payload's model and usage become the trailers (moot's deepseek)"
        (ev "prompt" {"hook_event_name" "UserPromptSubmit" "session_id" "d1" "cwd" proj "prompt" "moot x · round 1" "model" "deepseek-flash"})
        (ev "stop" {"hook_event_name" "Stop" "session_id" "d1" "cwd" proj "last_assistant_message" "use f"
                    "model" "deepseek-flash" "usage" {"input_tokens" 30 "output_tokens" 7 "cache_read_input_tokens" 3}})
        (is (= "response 00" (git "log" "-1" "--format=%s" "proj/s/d1")))
        (is (= "use f" (git "show" "proj/s/d1:.claude-trace/reply.md")))
        (is (= ["deepseek-flash" "30" "7" "3"] (map #(trailer "proj/s/d1" %) ["Model" "Tokens-In" "Tokens-Out" "Cache-Read"]))))
      (testing "a payload with a transcript is not read for them"
        (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "p" "transcript_path" "/nonexistent.jsonl"})
        (ev "stop" {"session_id" "s1" "cwd" proj "last_assistant_message" "r" "transcript_path" "/nonexistent.jsonl"
                    "model" "m" "usage" {"input_tokens" 1}})
        (is (nil? (trailer "proj/s/s1" "Model")))))))
