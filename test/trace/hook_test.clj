(ns trace.hook-test
  "The hook as Claude Code runs it (a bb process per event) and the worker behind it, and what the
  2026-10-25 review found (F1..F13, .scratch/review/findings.md), each against throwaway repos under
  <reus>/.scratch/hook-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [trace.agents :as agents]
            [trace.disk :as disk]
            [trace.files :as files]
            [trace.git :as g :refer [git]]
            [trace.health :as health]
            [trace.hook :as hook]
            [trace.install :as install]
            [trace.record :as rec]
            [trace.store :as store]
            [trace.worker :as worker]))

(def code (str (fs/parent (System/getProperty "babashka.config"))))
(def scratch (str (fs/path code ".scratch" "hook-test")))
(def seen (str (fs/path scratch "seen")))

;; the hook's marks and trace:health's look at this machine's transcripts: not the real ones
(use-fixtures :each (fn [t] (binding [health/*claude-dir* (str (fs/path seen "no-claude")) health/*seen-dir* seen] (t))))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))
(defn- real [dir & args] (str/trim (:out (apply sh dir "git" "-c" "user.name=t" "-c" "user.email=t@t" args))))

(defn- repo! [dir]
  (fs/create-dirs dir)
  (sh dir "git" "init" "-q" "-b" "main")
  (spit (str (fs/path dir "a.txt")) "a\n")
  (sh dir "git" "add" ".")
  (real dir "commit" "-q" "-m" "init")
  (str dir))

(def ^:private dir (atom scratch))

(defn- fresh!
  "A dir of the test's own: a worker woken by an earlier test may still be idling on that one's repo."
  []
  (when (= @dir scratch) (fs/delete-tree scratch))
  (reset! dir (str (fs/path scratch (str (System/nanoTime)))))
  (fs/create-dirs @dir))

(defn- ev [event m] (rec/handle! event m))
(defn- bash [sid cwd c] (ev "tool" {"session_id" sid "cwd" cwd "tool_name" "Bash" "tool_input" {"command" c} "tool_response" "ok"}))
(defn- subjects [b] (str/split-lines (git "log" "--format=%s" b)))
(defn- files-of [rev] (set (remove #(str/starts-with? % ".claude-trace/") (str/split-lines (git "ls-tree" "-r" "--name-only" rev)))))
(defn- trailer [rev k] (second (re-find (re-pattern (str "(?m)^" k ": (.+)$")) (git "log" "-1" "--format=%B" rev))))
(defn- branches [] (set (str/split-lines (git "for-each-ref" "--format=%(refname:short)" "refs/heads"))))

;; --- the hook process, the spool, the worker -------------------------------------------------

(defn- hook-run
  "The hook as Claude Code runs it; ms it took."
  [repo event m & [env]]
  (let [t0 (System/nanoTime)]
    (p/shell {:in (some-> m json/generate-string) :out :string :err :string
              :extra-env (merge {"TRACE_REPO" repo "TRACE_SEEN" seen} env)}
             "bb" "--config" (str (fs/path code "bb.edn")) "-m" "trace.hook" event)
    (/ (- (System/nanoTime) t0) 1e6)))

(defn- drained? [repo ms]
  (loop [n 0]
    (cond (empty? (files/entries (files/spool-dir repo))) true
          (> (* n 50) ms) false
          :else (do (Thread/sleep 50) (recur (inc n))))))

(deftest the-hook-spools-and-the-worker-records
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (hook-run repo "prompt" {"session_id" "h1" "cwd" proj "prompt" "go" "hook_event_name" "UserPromptSubmit"})
    (spit (str (fs/path proj "a.txt")) "a2\n")
    (let [ms (hook-run repo "tool" {"session_id" "h1" "cwd" proj "hook_event_name" "PostToolUse" "tool_name" "Edit"
                                    "tool_input" {"file_path" (str (fs/path proj "a.txt"))} "tool_response" "ok"})]
      (spit (str (fs/path proj "a.txt")) "a3, after the call\n")
      (hook-run repo "stop" {"session_id" "h1" "cwd" proj "hook_event_name" "Stop" "last_assistant_message" "done"})
      (is (drained? repo 15000) "the worker the hook woke recorded the spool")
      (binding [g/*repo* repo]
        (is (= ["response 00" "00.1 Edit: a.txt" "prompt 00: go"] (take 3 (subjects "proj/s/h1"))))
        (testing "the tree is the worktree as it was right after the call, not when the worker got to it"
          (is (= "a2" (git "show" "proj/s/h1~1:a.txt"))))
        (is (= "trace ok" (health/line)))
        (is (< ms 2000) "a tool hook returns without waiting for the worker")))))

(deftest a-failing-event-goes-aside-and-the-rest-go-on
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (hook/ensure-repo! repo)
      (hook/spool! repo {:event "prompt" :raw (json/generate-string {"session_id" "f1" "cwd" proj "prompt" "go"})
                         :obs (hook/observe! repo "prompt" {"session_id" "f1" "cwd" proj "prompt" "go"})})
      (hook/spool! repo {:event "tool" :raw (json/generate-string {"session_id" "f1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"}})
                         :obs {:wt {:project "proj" :root proj :common (str proj "/.git") :snap {:base "0000000000000000000000000000000000000bad"}}}})
      (hook/spool! repo {:event "stop" :raw (json/generate-string {"session_id" "f1" "cwd" proj "last_assistant_message" "ok"}) :obs {}})
      (is (= 2 (worker/drain! repo)))
      (is (= 1 (count (files/entries (files/failed-dir repo)))))
      (is (str/includes? (health/line) "1 in .git/trace-failed"))
      (is (= ["response 00" "prompt 00: go"] (take 2 (subjects "proj/s/f1"))))
      (testing "retry spools it again: its prompt is closed by now, so it records nothing and is gone"
        (is (= 1 (worker/retry! repo)))
        (worker/drain! repo)
        (is (empty? (files/entries (files/failed-dir repo))))
        (is (= "trace ok" (health/line)))))))

(deftest behind-is-visible
  (fresh!)
  (let [repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (hook/ensure-repo! repo)
      (let [f (hook/spool! repo {:event "stop" :raw "{}" :obs {}})
            old (fs/path (fs/parent f) (format "%019d-1.edn" (* (- (System/currentTimeMillis) 120000) 1000000)))]
        (fs/move f old)
        (is (str/includes? (health/line) "trace behind since"))
        (is (str/starts-with? (health/short) "TRACE BEHIND"))
        (worker/drain! repo)
        (is (= "trace ok" (health/line)))))))

;; --- F1: an OS lock; a dead holder holds nothing ----------------------------------------------

(deftest f1-a-dead-lock-holder-wedges-nothing
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        lock (str (fs/path repo ".git" "trace-write.lock"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "l1" "cwd" proj "prompt" "go"})
      (let [holder (p/process {:out :string :err :string}
                              "bb" "--config" (str (fs/path code "bb.edn")) "-e"
                              (str "(require 'trace.files) (trace.files/with-file-lock \"" lock "\" nil #(do (println :held) (flush) (Thread/sleep 60000)))"))]
        (Thread/sleep 1500)
        (testing "while a live process holds it, nobody takes it over"
          (is (nil? (files/try-file-lock lock (constantly :got)))))
        (p/shell "kill" "-9" (str (.pid (:proc holder))))
        (Thread/sleep 200)
        (let [t0 (System/currentTimeMillis)]
          (bash "l1" proj "ls")
          (is (< (- (System/currentTimeMillis) t0) 3000) "killed holder: the next event goes on at once")
          (is (= "00.1 Bash: ls" (first (subjects "proj/s/l1")))))))))

;; --- F2: the last mirrored commit gone from the repo ------------------------------------------

(deftest f2-mirror-base-gone
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "m1" "cwd" proj "prompt" "go"})
      (sh proj "git" "checkout" "-q" "--orphan" "fresh")
      (real proj "commit" "-q" "-m" "rewritten")
      (sh proj "git" "branch" "-f" "main" "fresh")
      (sh proj "git" "checkout" "-q" "main")
      (sh proj "git" "branch" "-D" "fresh")
      (sh proj "git" "reflog" "expire" "--expire=now" "--all")
      (sh proj "git" "gc" "-q" "--prune=now")
      (bash "m1" proj "ls")
      (ev "stop" {"session_id" "m1" "cwd" proj "last_assistant_message" "ok"})
      (is (= ["response 00" "00.1 Bash: ls"] (take 2 (subjects "proj/s/m1"))) "the events still record")
      (is (= "rewritten" (git "log" "-1" "--format=%s" "proj/main")) "the mirror goes on from the new tip"))))

;; --- F3 / F4: a stale index lock, an index naming a blob the trace repo lacks -------------------

(deftest f3-f4-snapshot-heals
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        idx (str (fs/path repo ".git" "trace-index" (disk/project-name proj)))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "i1" "cwd" proj "prompt" "go"})
      (testing "a git killed while writing the index left its lock: removed, the tree is right"
        (spit (str idx ".lock") "")
        (spit (str (fs/path proj "a.txt")) "v2\n")
        (bash "i1" proj "ls")
        (is (= "v2" (git "show" "proj/s/i1:a.txt"))))
      (testing "an index naming a blob this repo does not have: rebuilt, the tree is right"
        (let [b (str/trim (:out (p/shell {:dir proj :in "only in the project\n" :out :string} "git" "hash-object" "-w" "--stdin")))]
          (p/shell {:dir proj :extra-env {"GIT_INDEX_FILE" idx} :out :string :err :string}
                   "git" "update-index" "--add" "--cacheinfo" (str "100644," b ",ghost.txt")))
        (spit (str (fs/path proj "a.txt")) "v3\n")
        (bash "i1" proj "ls")
        (is (= "v3" (git "show" "proj/s/i1:a.txt")))
        (is (not ((files-of "proj/s/i1") "ghost.txt")))
        (is (nil? (trailer "proj/s/i1" "Snapshot-Error"))))
      (testing "git that fails for real is no silent success: the commit says so"
        (let [f (str (fs/path repo ".git" "objects"))]
          (with-redefs [disk/live-snapshot (fn [& _] (throw (ex-info "snapshot of x failed: boom" {})))]
            (bash "i1" proj "ls"))
          (is (= "snapshot of x failed: boom" (trailer "proj/s/i1" "Snapshot-Error")))
          (is (fs/exists? f)))))))

;; --- F6: an agent's calls without its SubagentStart --------------------------------------------

(deftest f6-agent-without-start
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "a1" "cwd" proj "prompt" "go"})
      (ev "tool" {"session_id" "a1" "agent_id" "zz9" "agent_type" "Explore" "cwd" proj "tool_name" "Bash"
                  "tool_input" {"command" "ls"} "tool_response" "ok"})
      (ev "subagent-stop" {"session_id" "a1" "agent_id" "zz9" "last_assistant_message" "found it"})
      (is (= ["response 00-azz9" "00-azz9.1 Bash: ls" "prompt 00-azz9: Explore: (the agent's start was not recorded)"]
             (take 3 (subjects "proj/a/a1-zz9")))))))

;; --- F7: a call in another project --------------------------------------------------------------

(deftest f7-a-call-in-another-project
  (fresh!)
  (let [alpha (repo! (fs/path @dir "alpha")) omega (repo! (fs/path @dir "omega")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "x1" "cwd" alpha "prompt" "work in both"})
      (spit (str (fs/path omega "o.txt")) "o\n")
      (ev "tool" {"session_id" "x1" "cwd" alpha "tool_name" "Write" "tool_input" {"file_path" (str (fs/path omega "o.txt"))} "tool_response" "ok"})
      (real omega "add" ".")
      (real omega "commit" "-q" "-m" "omega work")
      (bash "x1" alpha (str "cd " omega " && make"))
      (bash "x1" alpha "ls")
      (spit (str (fs/path alpha "a.txt")) "a2\n")
      (ev "tool" {"session_id" "x1" "cwd" alpha "tool_name" "Edit" "tool_input" {"file_path" (str (fs/path alpha "a.txt"))} "tool_response" "ok"})
      (ev "stop" {"session_id" "x1" "cwd" alpha "last_assistant_message" "both done"})
      (testing "the calls in omega are on omega's branch of the session, with omega's tree and HEAD"
        (is (= ["response 00" "00.3 Bash:" "00.2 Bash:" "00.1 Write:" "prompt 00:"] (map #(re-find #"^\S+ \S+" %) (take 5 (subjects "omega/s/x1")))))
        (is (= #{"a.txt" "o.txt"} (files-of "omega/s/x1~2")))
        (is (= (real omega "rev-parse" "HEAD") (trailer "omega/s/x1~2" "Real-Head")))
        (is (= "o.txt" (trailer "omega/s/x1~3" "File"))))
      (testing "a read in alpha after that moves nothing: it stays where the session wrote last"
        (is (= "00.3 Bash: ls" (second (subjects "omega/s/x1")))))
      (testing "the write back in alpha is on alpha's branch, numbered on"
        (is (= ["response 00" "00.4 Edit: a.txt" "prompt 00: work in both"] (take 3 (subjects "alpha/s/x1"))))
        (is (= "a2" (git "show" "alpha/s/x1~1:a.txt"))))
      (testing "omega's main is mirrored too"
        (is (= "omega work" (git "log" "-1" "--format=%s" "omega/main")))))))

;; --- F8: two checkouts with the same name -------------------------------------------------------

(deftest f8-same-name-different-repos
  (fresh!)
  (let [x (repo! (fs/path @dir "x" "foo")) y (repo! (fs/path @dir "y" "foo")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "n1" "cwd" x "prompt" "in x"})
      (ev "prompt" {"session_id" "n2" "cwd" y "prompt" "in y"})
      (let [bs (branches) other (first (filter #(re-matches #"foo-[0-9a-f]{6}/s/n2" %) bs))]
        (is (bs "foo/s/n1"))
        (is other "the second repo named foo gets a name of its own")
        (is (= (real y "rev-parse" "HEAD") (trailer (str/replace other #"/s/n2$" "/main") "Real-Commit")))))))

;; --- F9: a mirror catch-up cut off half-way ------------------------------------------------------

(deftest f9-mirror-resumes-without-duplicates
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        main-subjects #(str/split-lines (git "log" "--format=%s" "proj/main"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "r1" "cwd" proj "prompt" "go"})
      (doseq [i (range 3)] (spit (str (fs/path proj "a.txt")) (str i)) (real proj "commit" "-qam" (str "real " i)))
      (testing "a mirror that fails half-way goes on from where it got to at the next event"
        (let [orig disk/commit-info n (atom 0)]
          (with-redefs [disk/commit-info (fn [d p sha] (when (= 2 (swap! n inc)) (throw (ex-info "boom" {}))) (orig d p sha))]
            (bash "r1" proj "make")))
        (is (= ["real 0" "init"] (main-subjects)))
        (store/kv! (store/live repo) :mirror-failed "proj" {:tip (real proj "rev-parse" "HEAD") :at 0}) ; as after a while
        (bash "r1" proj "make")
        (is (= ["real 2" "real 1" "real 0" "init"] (main-subjects))))
      (testing "cut off between a mirror commit and its state: the trace main tip's Real-Commit says it is done"
        (spit (str (fs/path proj "a.txt")) "3") (real proj "commit" "-qam" "real 3")
        (bash "r1" proj "make")
        (store/kv! (store/live repo) :mirror "proj" (real proj "rev-parse" "HEAD~1"))
        (bash "r1" proj "make")
        (is (= ["real 3" "real 2" "real 1" "real 0" "init"] (main-subjects)))))))

;; --- F10 / F11: the hook does not wait for others, nor for the transcript ------------------------

(deftest f10-f11-the-hook-waits-for-nobody
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        lock (str (fs/path repo ".git" "trace-write.lock"))
        tr (str (fs/path @dir "t.jsonl"))]
    (spit tr "")
    (hook/ensure-repo! repo)
    (let [holder (p/process {:out :string :err :string}
                            "bb" "--config" (str (fs/path code "bb.edn")) "-e"
                            (str "(require 'trace.files) (trace.files/with-file-lock \"" lock "\" nil #(Thread/sleep 20000))"))]
      (try
        (Thread/sleep 1000)
        (let [prompt-ms (hook-run repo "prompt" {"session_id" "w1" "cwd" proj "prompt" "not in the transcript yet" "transcript_path" tr})
              tool-ms (hook-run repo "tool" {"session_id" "w1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"}
                                             "tool_response" "ok" "transcript_path" tr})]
          (is (< prompt-ms 1500) "a prompt hook does not wait for its text to reach the transcript, nor for the lock")
          (is (< tool-ms 1500) "a tool hook does not wait for the writer"))
        (finally (p/destroy-tree holder))))
    (is (drained? repo 15000))))

;; --- F12: an agent's transcript beside its session's -------------------------------------------

(deftest f12-agent-file-without-a-search
  (fresh!)
  (let [tr (fs/path @dir "projects" "p" "s1.jsonl")
        af (fs/path @dir "projects" "p" "s1" "subagents" "agent-ab.jsonl")]
    (fs/create-dirs (fs/parent af))
    (spit (str tr) "") (spit (str af) "")
    (with-redefs [fs/glob (fn [& _] (throw (ex-info "searched" {})))]
      (is (= (str af) (str (agents/find-file "s1" "ab" ".jsonl" (str tr))))))))

;; --- F13: outside git nothing is hashed ----------------------------------------------------------

(deftest f13-outside-git-nothing-is-hashed
  (fresh!)
  (let [root (str (fs/path @dir "projects")) bulk (str (fs/path root "bulk")) repo (str (fs/path @dir "traces"))]
    (fs/create-dirs (fs/path bulk "data"))
    (spit (str (fs/path bulk "data" "big.bin")) (apply str (repeat 100000 "x")))
    (with-redefs [disk/projects-root (constantly root)]
      (binding [g/*repo* repo g/*env* {"GIT_CEILING_DIRECTORIES" root}] ; scratch sits in the reus checkout
        (ev "prompt" {"session_id" "b1" "cwd" bulk "prompt" "look"})
        (bash "b1" bulk "ls data")
        (is (= ["00.1 Bash: ls data" "prompt 00: look"] (take 2 (subjects "bulk/s/b1"))))
        (is (empty? (files-of "bulk/s/b1")) "the dir's files are not in the trace")))))

;; --- state kept by older code ----------------------------------------------------------------------

(deftest older-session-state-goes-on
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "o1" "cwd" proj "prompt" "first"})
      (bash "o1" proj "ls")
      (ev "stop" {"session_id" "o1" "cwd" proj "last_assistant_message" "ok"})
      (let [s (store/live repo)]
        (store/kv! s :sessions "o1" {:project "proj" :root proj :troot proj :common (str proj "/.git") :git-dir (str proj "/.git")
                                     :branch "proj/s/o1" :nn "00" :k 1 :open? false :session "o1"})
        (store/kv! s :counters "proj/s/o1" 0))
      (ev "prompt" {"session_id" "o1" "cwd" proj "prompt" "second"})
      (bash "o1" proj "ls")
      (is (= ["01.1 Bash: ls" "prompt 01: second" "response 00"] (take 3 (subjects "proj/s/o1")))))))

;; --- round 2 (.scratch/review/findings2.md) -------------------------------------------------------

(deftest g1-an-entry-recorded-before-a-cut-off-is-not-recorded-again
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (hook/ensure-repo! repo)
      (let [spool (fn [event m] (hook/spool! repo {:event event :raw (json/generate-string m) :obs (hook/observe! repo event m)}))
            _ (spool "prompt" {"session_id" "d1" "cwd" proj "prompt" "go"})
            f (spool "tool" {"session_id" "d1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "make"} "tool_response" "ok"})
            body (slurp f)]
        (is (= 2 (worker/drain! repo)))
        (spit f body) ; the worker recorded it and was killed before it could delete it
        (worker/drain! repo)
        (is (= ["00.1 Bash: make" "prompt 00: go"] (take 2 (subjects "proj/s/d1"))) "no second 00.2 for the same call")
        (is (empty? (files/entries (files/spool-dir repo))))))))

(deftest g2-mirror-after-the-project-pruned-the-tree-it-had
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "m2" "cwd" proj "prompt" "go"})
      (spit (str (fs/path proj "a.txt")) "only once\n") (real proj "commit" "-qam" "soon gone")
      (bash "m2" proj "make")
      (real proj "reset" "-q" "--hard" "HEAD~1")
      (spit (str (fs/path proj "b.txt")) "b\n") (real proj "add" ".") (real proj "commit" "-qm" "after the rewrite")
      (sh proj "git" "reflog" "expire" "--expire=now" "--all")
      (sh proj "git" "gc" "-q" "--prune=now")
      (spit (str (fs/path proj "untracked.txt")) "so the snapshot is not main's tree\n")
      (bash "m2" proj "make")
      (is (= "after the rewrite" (git "log" "-1" "--format=%s" "proj/main")))
      (is (not (fs/exists? (fs/path repo ".git" "trace-errors.log")))))))

(deftest g3-a-read-elsewhere-in-the-project-moves-nothing
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces")) wt (str (fs/path @dir "wt"))]
    (real proj "worktree" "add" "-q" wt "-b" "x")
    (spit (str (fs/path wt "only-on-x.txt")) "x\n")
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "g3" "cwd" wt "prompt" "work on x"})
      (spit (str (fs/path wt "a.txt")) "x1\n")
      (ev "tool" {"session_id" "g3" "cwd" wt "tool_name" "Edit" "tool_input" {"file_path" (str (fs/path wt "a.txt"))} "tool_response" "ok"})
      (bash "g3" wt (str "git -C " proj " log"))
      (ev "tool" {"session_id" "g3" "cwd" wt "tool_name" "Read" "tool_input" {"file_path" (str (fs/path proj "a.txt"))} "tool_response" "ok"})
      (ev "stop" {"session_id" "g3" "cwd" wt "last_assistant_message" "ok"})
      (ev "prompt" {"session_id" "g3" "cwd" proj "prompt" "again"})
      (spit (str (fs/path wt "a.txt")) "x2\n")
      (ev "tool" {"session_id" "g3" "cwd" proj "tool_name" "Edit" "tool_input" {"file_path" (str (fs/path wt "a.txt"))} "tool_response" "ok"})
      (let [wt-head (real wt "rev-parse" "HEAD")]
        (is (every? #(= wt-head %) (map #(trailer (str "proj/s/g3~" %) "Real-Head") (range 0 5))) "all of it in the worktree")
        (is (= ["a.txt"] (str/split-lines (git "diff" "--name-only" "proj/s/g3~1" "proj/s/g3" "--" "." ":(exclude).claude-trace")))
            "the edit back in the worktree changes the one file")))))

(deftest g6-garbage-on-stdin-is-no-event
  (fresh!)
  (let [repo (str (fs/path @dir "traces"))]
    (p/shell {:in "this is not json" :out :string :err :string :extra-env {"TRACE_REPO" repo "TRACE_SEEN" seen}}
             "bb" "--config" (str (fs/path code "bb.edn")) "-m" "trace.hook" "tool")
    (binding [g/*repo* repo]
      (is (empty? (files/entries (files/spool-dir repo))))
      (is (= "trace ok" (health/line)))
      (is (str/includes? (slurp (str (fs/path repo ".git" "trace-errors.log"))) "no session_id")))))

(deftest s1-an-unreadable-entry-goes-aside
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))]
    (binding [g/*repo* repo]
      (hook/ensure-repo! repo)
      (spit (hook/spool! repo {:event "prompt" :raw "{}" :obs {}}) "{:event \"prompt\" :raw ")
      (hook/spool! repo {:event "prompt" :raw (json/generate-string {"session_id" "u1" "cwd" proj "prompt" "after it"})
                         :obs (hook/observe! repo "prompt" {"session_id" "u1" "cwd" proj "prompt" "after it"})})
      (is (= 1 (worker/drain! repo)))
      (is (= 1 (count (files/entries (files/failed-dir repo)))))
      (is (= "prompt 00: after it" (first (subjects "proj/s/u1")))))))

(deftest own-objects-makes-an-older-repo-self-contained
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        alt (fs/path repo ".git" "objects" "info" "alternates")]
    (binding [g/*repo* repo]
      (ev "prompt" {"session_id" "o2" "cwd" proj "prompt" "go"})
      ;; what older code made: a commit whose blob only the project has, reachable through alternates
      (spit (str (fs/path proj "borrowed.txt")) "only in the project\n")
      (real proj "add" ".") (real proj "commit" "-qm" "borrowed")
      (spit (str alt) (str proj "/.git/objects\n"))
      (let [t (real proj "rev-parse" "HEAD^{tree}")
            c (git "commit-tree" t "-p" "proj/main" "-m" "older code")]
        (git "update-ref" "refs/heads/proj/main" c))
      (hook/spool! repo {:event "stop" :raw (json/generate-string {"session_id" "o2" "cwd" proj "last_assistant_message" "ok"}) :obs {}})
      (worker/own-objects! repo)
      (is (not (fs/exists? alt)))
      (is (fs/exists? (str alt ".bak")))
      (fs/delete-tree proj)
      (is (zero? (:exit (g/git* {} "fsck" "--connectivity-only" "--no-dangling"))) "complete with the project gone")
      (is (empty? (files/entries (files/spool-dir repo))) "the waiting event was recorded after")
      (is (= "response 00" (first (subjects "proj/s/o2")))))))

(deftest p1-the-hook-line-needs-no-path
  (fresh!)
  (let [proj (repo! (fs/path @dir "proj")) repo (str (fs/path @dir "traces"))
        sid (str "p1-" (System/nanoTime))
        line (get-in (install/hook-entries (str (fs/path code "bb.edn"))) ["UserPromptSubmit" 0 "hooks" 0 "command"])
        run (fn [cmd m] (p/shell {:in (some-> m json/generate-string) :out :string :err :string :continue true
                                  :env {"PATH" "" "TRACE_REPO" repo "TRACE_SEEN" seen}}
                                 "/bin/sh" "-c" cmd))]
    (is (= 127 (:exit (run "bb --version" nil))) "the session's PATH has no bb (nor git)")
    (is (zero? (:exit (run line {"session_id" sid "cwd" proj "prompt" "go" "hook_event_name" "UserPromptSubmit"}))))
    (is (drained? repo 15000) "the worker it woke, git included, ran without the session's PATH")
    (binding [g/*repo* repo]
      (is (= "prompt 00: go" (first (subjects (str "proj/s/" (subs sid 0 8))))))
      (is (= "trace ok" (health/line))))
    (is (health/seen? sid) "the hook marked the session")))

(deftest p2-a-session-the-hook-never-ran-for-is-seen
  (fresh!)
  (let [claude (str (fs/path @dir "claude")) marks (str (fs/path @dir "seen"))
        t (fn [f ago-ms] (fs/create-dirs (fs/parent f)) (spit (str f) "{}\n")
            (fs/set-last-modified-time f (fs/millis->file-time (- (System/currentTimeMillis) ago-ms))) f)
        m 60000]
    (fs/create-dirs claude)
    (spit (str (fs/path claude "settings.json")) (json/generate-string {"hooks" (install/hook-entries "/r/bb.edn")}))
    (binding [health/*claude-dir* claude health/*seen-dir* marks g/*repo* (str (fs/path @dir "traces"))]
      (t (fs/path claude "projects" "-p" "old.jsonl") (* 3 3600000))
      (is (empty? (health/unrecorded)) "nothing before the hook marked a first session")
      (health/seen! "first")
      (t (fs/path marks ".since") (* 2 3600000))
      (t (fs/path claude "projects" "-p" "first.jsonl") (* 10 m))
      (t (fs/path claude "projects" "-p" "lost.jsonl") (* 5 m))
      (t (fs/path claude "projects" "-q" "busy.jsonl") 1000)
      (t (fs/path claude "projects" "-p" "first" "subagents" "agent-x.jsonl") (* 5 m))
      (let [touched (fs/path claude "projects" "-p" "touched.jsonl")]
        (t touched (* 5 m))
        (spit (str touched) "{\"type\":\"system\",\"timestamp\":\"2020-01-01T00:00:00.000Z\"}\n")
        (fs/set-last-modified-time touched (fs/millis->file-time (- (System/currentTimeMillis) (* 5 m)))))
      (is (= ["lost"] (map :session (health/unrecorded)))
          "not one the hook ran for, nor one written in the last minute, nor one older than the marks, nor an old one only touched")
      (is (str/includes? (health/line) "trace sessions not recorded"))
      (is (str/includes? (health/short) "lost"))
      (is (= ["lost"] (map :session (health/ack!))))
      (is (= "trace ok" (health/line)) "acked")
      (t (fs/path claude "projects" "-p" "lost2.jsonl") (* 5 m))
      (spit (str (fs/path claude "settings.json")) "{}")
      (is (empty? (health/unrecorded)) "the hook is off (trace:off): nothing is missed"))))
