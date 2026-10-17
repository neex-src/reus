(ns trace.replay-test
  "trace.replay of a synthetic transcript against a small project under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.git :as g :refer [git]]
            [trace.read :as r]
            [trace.replay :as rp]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "replay-test")))
(def orig "/nonexistent-replay/proj")

(defn- sh [dir env & args] (apply p/shell {:dir (str dir) :out :string :err :string :extra-env env} args))

(defn- commit! [proj date files msg]
  (doseq [[f c] files] (spit (str (fs/path proj f)) c))
  (sh proj {} "git" "add" ".")
  (sh proj {"GIT_AUTHOR_DATE" date "GIT_COMMITTER_DATE" date}
      "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" msg))

(defn- jsonl! [f entries]
  (fs/create-dirs (fs/parent f))
  (spit (str f) (str/join "\n" (map json/generate-string entries))))

(defn- u [ts m] (merge {"type" "user" "timestamp" ts "cwd" orig "sessionId" "s1" "gitBranch" "main"} m))
(defn- a [ts content] {"type" "assistant" "timestamp" ts "cwd" orig "sessionId" "s1"
                       "message" {"id" (str "m" ts) "content" content}})
(defn- use* [id tool input] {"type" "tool_use" "id" id "name" tool "input" input})
(defn- res [id text] {"message" {"content" [{"type" "tool_result" "tool_use_id" id "content" text}]}})

(defn- fixture! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "real" "proj")
        f (fs/path scratch "projects" "p" "s1.jsonl")]
    (fs/create-dirs proj)
    (sh proj {} "git" "init" "-q" "-b" "main")
    (commit! proj "2026-01-01T10:00:00Z" {"a.clj" "v1\n"} "one")
    (commit! proj "2026-01-01T12:00:00Z" {"a.clj" "v1x\n"} "two")
    (jsonl! f
            [(u "2026-01-01T11:00:00.000Z" {"message" {"content" "look at a"}})
             (a "2026-01-01T11:00:01.000Z" [(use* "t1" "Read" {"file_path" (str orig "/a.clj")})])
             (u "2026-01-01T11:00:02.000Z"
                (merge (res "t1" "v1")
                       {"toolUseResult" {"type" "text" "file" {"filePath" (str orig "/a.clj") "content" "v1\n"
                                                               "numLines" 1 "startLine" 1 "totalLines" 1}}}))
             (a "2026-01-01T11:00:03.000Z" [(use* "t2" "Edit" {"file_path" (str orig "/a.clj") "old_string" "v1" "new_string" "v1x"})])
             (u "2026-01-01T11:00:04.000Z"
                (merge (res "t2" "ok") {"toolUseResult" {"filePath" (str orig "/a.clj") "originalFile" "v1\n"}}))
             (a "2026-01-01T11:00:05.000Z" [(use* "t3" "Bash" {"command" "echo hello > b.txt"})])
             (u "2026-01-01T11:00:06.000Z"
                (merge (res "t3" "")
                       {"toolUseResult" {"stdout" "" "stderr" ""
                                         "bashEditDiff" {"files" [{"filePath" (str orig "/b.txt")
                                                                   "hunks" [{"oldStart" 1 "oldLines" 0 "newStart" 1 "newLines" 1
                                                                             "lines" ["+hello"]}]}]}}}))
             (a "2026-01-01T11:00:07.000Z" [(use* "t4" "Agent" {"prompt" "check"})])
             (u "2026-01-01T11:00:08.000Z" (merge (res "t4" "agentId: ag1") {"toolUseResult" {"agentId" "ag1"}}))
             (a "2026-01-01T11:00:09.000Z" [{"type" "text" "text" "edited a"}])
             {"type" "system" "subtype" "stop_hook_summary" "timestamp" "2026-01-01T11:00:10.000Z"}
             (u "2026-01-01T12:30:00.000Z" {"message" {"content" "and now?"}})
             (a "2026-01-01T12:30:01.000Z" [(use* "t5" "Bash" {"command" "cat a.clj"})])
             (u "2026-01-01T12:30:02.000Z" (merge (res "t5" "v1x") {"toolUseResult" {"stdout" "v1x" "stderr" ""}}))
             (a "2026-01-01T12:30:03.000Z" [(use* "t6" "Bash" {"command" "false"})])
             (u "2026-01-01T12:30:04.000Z" {"message" {"content" [{"type" "tool_result" "tool_use_id" "t6"
                                                                    "content" "Exit code 1" "is_error" true}]}})
             (a "2026-01-01T12:30:05.000Z" [{"type" "text" "text" "still v1x"}])])
    (jsonl! (fs/path scratch "projects" "p" "s1" "subagents" "agent-ag1.jsonl")
            [{"type" "user" "timestamp" "2026-01-01T11:00:07.500Z" "cwd" orig "sessionId" "s1" "isSidechain" true
              "message" {"content" "check"}}
             {"type" "assistant" "timestamp" "2026-01-01T11:00:07.600Z" "cwd" orig "sessionId" "s1"
              "message" {"content" [(use* "x1" "Bash" {"command" "ls"})]}}
             {"type" "user" "timestamp" "2026-01-01T11:00:07.700Z" "cwd" orig "sessionId" "s1"
              "message" {"content" [{"type" "tool_result" "tool_use_id" "x1" "content" "a.clj"}]}
              "toolUseResult" {"stdout" "a.clj" "stderr" ""}}
             {"type" "assistant" "timestamp" "2026-01-01T11:00:07.800Z" "cwd" orig "sessionId" "s1"
              "message" {"content" [{"type" "text" "text" "all fine"}]}}])
    (spit (str (fs/path scratch "projects" "p" "s1" "subagents" "agent-ag1.meta.json")) "{\"agentType\":\"Explore\"}")
    [(str f) (str proj)]))

(deftest patch-hunks
  (is (= ["hello\n" 0] (rp/patch nil [{"oldStart" 1 "lines" ["+hello"]}])))
  (is (= ["a\nB\nc\n" 0] (rp/patch "a\nb\nc\n" [{"oldStart" 2 "lines" ["-b" "+B"]}])))
  (is (= [nil 0] (rp/patch "a\n" [{"oldStart" 1 "lines" ["-a"]}])) "all lines gone: the file is gone")
  (is (= 1 (second (rp/patch "a\nz\n" [{"oldStart" 2 "lines" ["-b" "+B"]}]))) "a hunk that does not fit is counted"))

(deftest worktrees-named-by-commands
  (let [ev (fn [cmd] {:kind :tool :payload {"cwd" "/r/p" "tool_input" {"command" cmd}}})]
    (is (= [["/r/p/.scratch/wt-a" "feature/a"] ["/r/p/.scratch/wt-b" "chore/x"] ["/r/q/wt-c" nil] ["/r/p/x/wt-d" "d"]]
           (rp/worktree-adds [(ev "git worktree add .scratch/wt-a -b feature/a main 2>&1")
                              (ev "git worktree add -q -b chore/x .scratch/wt-b HEAD")
                              (ev "git -C /r/q worktree add --detach wt-c main >/dev/null")
                              (ev "cd x && git worktree add wt-d -b d")])))))

(deftest replay-a-session
  (let [[f proj] (fixture!)
        repo (str (fs/path scratch "replay"))
        stats (rp/replay! [f] repo {orig proj})]
    (binding [g/*repo* repo]
      (let [tips (r/tips)
            p0 (r/prompt-data tips "proj" "00")
            p1 (r/prompt-data tips "proj" "01")
            ag (r/prompt-data tips "proj" "00-aag1")]
        (is (= ["proj"] (r/mains tips)) "the project is named as the transcript spells it")
        (is (= ["00" "01"] (r/prompt-nos tips "proj")))
        (is (= "look at a" (:prompt p0)))
        (is (= "edited a" (:reply p0)))
        (is (= [:read :edit :read :run] (map :kind (:steps p0))) "echo > b.txt names b.txt, which is there now")
        (testing "the read is the file as it was at 11:00 (commit one)"
          (is (= "v1\n" (:out (g/git* {} "cat-file" "-p" (:blob (first (:steps p0))))))))
        (testing "the edit's diff is the edit"
          (is (= [["files/a.clj" 1 1]] (map (juxt :path :add :del) (:files (second (:steps p0))))))
          (is (= "v1\n" (:out (g/git* {} "show" (str (git "log" "-1" "--format=%H" "--grep=^baseline" "proj-p00") ":files/a.clj"))))
              "baseline from originalFile"))
        (testing "the bash edit diff reached the shadow: the next command reads b.txt as it holds"
          (is (= "hello\n" (:out (g/git* {} "cat-file" "-p" (:blob (nth (:steps p0) 2)))))))
        (testing "at 12:30 HEAD is commit two; the edit is in it"
          (is (= "v1x\n" (:out (g/git* {} "cat-file" "-p" (:blob (first (:steps p1)))))))
          (is (:err (second (:steps p1))) "the failure is marked"))
        (testing "the agent is its own branch"
          (is (= ["ls"] (map #(get-in % [:call :code]) (:steps ag))))
          (is (= "all fine" (:reply ag))))
        (testing "git dates are the transcript's"
          (is (= "1767265200" (git "log" "-1" "--format=%at" "--grep=^prompt 00:" "proj-p00")) "prompt 00 at 11:00Z"))
        (is (pos? (get-in stats [:head :reflog] 0)) "HEAD came from the project's reflog")
        (is (empty? (r/live-branches)))))))
