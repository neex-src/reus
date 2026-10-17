(ns trace.e2e
  "bb trace:e2e: the whole chain against a real Claude Code session, for new sessions on new repos.

  A fresh project under <reus>/.scratch/e2e/<timestamp>/proj, a fresh trace repo beside it
  (TRACE_REPO, so the live one is not touched), and `claude -p --model sonnet` told to: make a
  worktree .wt-feat on a branch feat, write src/a.txt there and commit, Edit it and commit, have an
  Explore agent read it, merge feat into main with --no-ff, Read README.md, and say done. The
  hooks are this checkout's (a settings file given to claude, the user's own hooks left out), so a
  branch is tested before it is merged. The recorded repo is then checked; each check prints
  pass or fail, the exit code is the number of failures. It costs tokens: not part of bb test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [trace.git :as g :refer [git]]
            [trace.install :as install]))

(def prompt
  (str "Do exactly these steps in this git repo, nothing else, no questions. Commit with "
       "`git -c user.name=t -c user.email=t@t commit`. "
       "1) Append .wt-feat to .git/info/exclude, then create a git worktree .wt-feat on a new branch feat. "
       "2) In the worktree write src/a.txt containing the single word one, and commit it there. "
       "3) With the Edit tool change src/a.txt in the worktree to the single word two, and commit it there. "
       "4) Use the Agent tool with subagent_type Explore to read the absolute path of .wt-feat/src/a.txt and tell you what it says. "
       "5) In the main checkout run: git merge --no-ff feat -m 'Merge feat'. "
       "6) Use the Read tool on README.md. "
       "7) Reply with the single word done."))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string :continue true} args))
(defn- out [dir & args] (str/trim (:out (apply sh dir args))))

(defn- run-claude! [dir proj repo bb-edn]
  (let [settings (str (fs/path dir "settings.json"))]
    (spit settings (json/generate-string {"hooks" (install/hook-entries bb-edn)}))
    (let [r (p/shell {:dir (str proj) :in prompt :out :string :err :string :continue true
                      :extra-env {"TRACE_REPO" repo}}
                     "claude" "-p" "--model" "sonnet" "--setting-sources" "project,local" "--settings" settings
                     "--allowedTools" "Bash" "Read" "Write" "Edit" "Agent")]
      (spit (str (fs/path dir "claude.out")) (str (:out r) "\n--- stderr ---\n" (:err r)))
      r)))

(defn- trailer [rev k]
  (second (re-find (re-pattern (str "(?m)^" k ": (.+)$")) (git "log" "-1" "--format=%B" rev))))

(defn- lines [s] (remove str/blank? (str/split-lines s)))

(defn checks
  "[[name ok? detail]] for the repo g/*repo* recorded from the project proj."
  [proj project]
  (let [refs (lines (git "for-each-ref" "--format=%(refname:short)" "refs/heads"))
        main (str project "/main")
        s (filter #(str/starts-with? % (str project "/s/")) refs)
        a (filter #(str/starts-with? % (str project "/a/")) refs)
        s1 (first s) a1 (first a)
        real-fp (count (lines (out proj "git" "rev-list" "--first-parent" "main")))
        trace-fp (count (lines (git "rev-list" "--first-parent" main)))
        real-merges (count (lines (out proj "git" "rev-list" "--first-parent" "--merges" "main")))
        mp (when (pos? trace-fp) (lines (str/replace (git "log" "-1" "--format=%P" main) " " "\n")))
        extra (rest mp)
        own (fn [b] (set (lines (git "rev-list" "--first-parent" b))))
        s-set (when s1 (own s1))
        a-set (when a1 (set/difference (own a1) (or s-set #{})))
        tool-commits (when s1 (lines (git "log" "--first-parent" "--format=%H" "--grep=^Tool: " s1)))
        noted (set (map #(second (str/split % #" ")) (lines (git "notes" "--ref=turn" "list"))))
        errlog (fs/path g/*repo* ".git" "trace-errors.log")
        feat (out proj "git" "rev-parse" "feat")
        agent-first (when a1 (first (lines (git "log" "--first-parent" "--reverse" "--format=%H" (str "^" s1) a1))))
        deletes (lines (git "log" "--no-merges" "--diff-filter=D" "--format=%h" "--all" "--" "src/a.txt"))
        adds (when s1 (lines (git "log" "--first-parent" "--diff-filter=A" "--format=%h" s1 "--" "src/a.txt")))
        reply (when s1 (git "show" (str s1 ":.claude-trace/reply.md")))
        brief (when a1 (git "show" (str a1 ":.claude-trace/prompt.md")))]
    [["refs: main, one session branch, one agent branch" (and (some #{main} refs) (= 1 (count s)) (= 1 (count a)))
      (pr-str refs)]
     ["mirror main = real main's first-parent count" (= real-fp trace-fp) (str "real " real-fp " trace " trace-fp)]
     ["mirror main has the real merge" (and (pos? real-merges) (> (count mp) 1)) (str "real merges " real-merges ", tip parents " (count mp))]
     ["the merge's extra parents are the session's and the agent's tips"
      (and (boolean (some s-set extra)) (boolean (some a-set extra))) (str (count extra) " extra parents")]
     ["no phantom delete of src/a.txt, added once on the session branch" (and (empty? deletes) (= 1 (count adds)))
      (str "deletes " (pr-str deletes) " adds " (pr-str adds))]
     ["agent brief is not empty" (and (not (str/blank? brief)) (not (str/ends-with? (git "log" "-1" "--format=%s" (first (lines (git "log" "--first-parent" "--reverse" "--format=%h" (str "^" s1) a1)))) "Explore:")))
      (pr-str (some-> brief (subs 0 (min 60 (count brief)))))]
     ["agent's first commit: Real-Head is the worktree's HEAD (feat), as its Cwd says"
      (and agent-first (str/ends-with? (str (trailer agent-first "Cwd")) ".wt-feat") (= feat (trailer agent-first "Real-Head")))
      (str "Cwd " (some-> agent-first (trailer "Cwd")) " Real-Head " (some-> agent-first (trailer "Real-Head")) " feat " feat)]
     ["every tool commit has a turn note" (and (seq tool-commits) (every? noted tool-commits))
      (str (count (filter noted tool-commits)) " of " (count tool-commits))]
     ["the last response has a reply" (not (str/blank? reply)) (pr-str reply)]
     ["no trace-errors.log lines" (or (not (fs/exists? errlog)) (str/blank? (slurp (str errlog))))
      (when (fs/exists? errlog) (slurp (str errlog)))]]))

(defn -main [& _]
  (let [root (fs/parent (System/getProperty "babashka.config"))
        bb-edn (str (System/getProperty "babashka.config"))
        dir (str (fs/path root ".scratch" "e2e" (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss") (java.time.LocalDateTime/now))))
        proj (str (fs/path dir "proj"))
        repo (str (fs/path dir "traces"))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "README.md")) "# e2e project\n")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (println "e2e in" dir)
    (let [r (run-claude! dir proj repo bb-edn)]
      (println "claude exit" (:exit r) "(output in claude.out)")
      (binding [g/*repo* repo]
        (if-not (fs/exists? repo)
          (do (println "FAIL nothing was recorded") (System/exit 99))
          (let [rs (checks proj "proj")]
            (doseq [[n ok? detail] rs] (println (if ok? "pass" "FAIL") "-" n (if ok? "" (str "  [" detail "]"))))
            (println (count (remove second rs)) "failed; trace repo" repo)
            (System/exit (count (remove second rs)))))))))
