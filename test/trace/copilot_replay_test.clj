(ns trace.copilot-replay-test
  "trace.copilot-replay: the evidence parsers, and a made-up two-prompt Copilot session (an edit, a here-document,
  a python script, an apply_patch, a rewind snapshot per prompt) replayed against a small project under
  <reus>/.scratch/copilot-replay-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.copilot-replay :as cr]
            [trace.disk :as disk]
            [trace.git :as g :refer [git]]
            [trace.shadow :as shadow]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "copilot-replay-test")))

(deftest apply-patch
  (let [files {"/p/a.js" "function f() {\n  x0\n  x\n  old\n  y\n}\n" "/p/gone.txt" "bye\n" "/p/m.txt" "1\n2\n"}
        [out misses] (cr/apply-patch (str "*** Begin Patch\n"
                                          "*** Update File: a.js\n@@ function f() {\n   x\n-  old\n+  new\n   y\n"
                                          "*** Add File: sub/b.txt\n+hi\n+there\n"
                                          "*** Delete File: gone.txt\n"
                                          "*** Update File: m.txt\n*** Move to: n.txt\n@@\n 1\n-2\n+two\n"
                                          "*** Update File: a.js\n@@\n-not there\n+x\n"
                                          "*** End Patch")
                                     "/p" files)]
    (is (= [["/p/a.js" "function f() {\n  x0\n  x\n  new\n  y\n}\n"]
            ["/p/sub/b.txt" "hi\nthere\n"]
            ["/p/gone.txt" nil]
            ["/p/m.txt" nil] ["/p/n.txt" "1\ntwo\n"]
            ["/p/a.js" "function f() {\n  x0\n  x\n  old\n  y\n}\n"]]
           out)
        "a chunk is found after its @@ line; a move removes the old path; a file is patched from its content before the patch")
    (is (= 1 misses) "the chunk whose old lines are nowhere is counted and left out")))

(deftest here-documents
  (is (= [["/p/bin/w.js" "#!/x $y\nline2\n" false]
          ["/p/README.md" "more\n" true]
          ["/p/a/b.txt" "z\n" false]
          ["/p/log.txt" "plain\n" true]]
         (cr/heredoc-writes (str "cd /p && mkdir -p bin && cat > bin/w.js <<'EOF'\n#!/x $y\nline2\nEOF\n"
                                 "chmod +x bin/w.js\n"
                                 "cat >> 'README.md' <<EOF\nmore\nEOF\n"
                                 "cat <<-'X' > a/b.txt\n\tz\n\tX\n"
                                 "tee -a log.txt <<E\nplain\nE\n"
                                 "tee -a log.txt <<E\n$expanded\nE\n"
                                 "python3 - <<'EOF'\nprint(1)\nEOF\n"
                                 "cat > $OUT <<'EOF'\nno\nEOF\n")
                            "/home"))
      "quoted documents as written; an unquoted one only without $, ` or \\; stdin of other programs and $paths left out"))

(deftest view-shows-a-whole-file
  (is (= "a\nb\n" (cr/view-text {"path" "/x"} {"content" "a\nb\n" "detailedContent" "diff\n@@ -1,3 +1,3 @@\n a\n b\n"})))
  (is (nil? (cr/view-text {"path" "/x" "view_range" [1 2]} {"content" "a\nb\n" "detailedContent" "@@ -1,3 +1,3 @@"})) "a range")
  (is (nil? (cr/view-text {"path" "/d"} {"content" "a\nb\n" "detailedContent" "create file mode 100644\n@@ -1,0 +1,3 @@"})) "a dir listing")
  (is (nil? (cr/view-text {"path" "/x"} {"content" "a\nb\n" "detailedContent" "@@ -1,9 +1,9 @@"})) "cut short"))

;; --- a whole session -----------------------------------------------------------------------

(defn- sh [dir env & args] (apply p/shell {:dir (str dir) :out :string :err :string :extra-env env} args))

(def sid "5e551011-aaaa-bbbb-cccc-000000000001")

(defn- log-entries
  "A Copilot events.jsonl in the shape trace.copilot-events reads, for project dir proj."
  [proj]
  (let [n (atom 0)
        e (fn [ts type data & [extra]] (merge {"type" type "data" data "id" (str "e" (swap! n inc)) "timestamp" (str "2026-01-01T" ts "Z")} extra))
        hook (fn [ts kind input] (e ts "hook.start" {"hookType" kind "input" input}))
        call (fn [ts1 ts2 id tool args result & [ok?]]
               [(e ts1 "tool.execution_start" {"toolCallId" id "toolName" tool "arguments" args})
                (e ts2 "tool.execution_complete" {"toolCallId" id "success" (if (false? ok?) false true) "result" result})])
        said (fn [ts text] (e ts "assistant.message" {"content" text "toolRequests" [] "model" "claude-haiku-5-5"}))]
    (concat
     [(e "10:00:00.000" "session.start" {"sessionId" sid "selectedModel" "claude-haiku-5-5" "context" {"cwd" proj}})
      (hook "10:00:01.000" "userPromptSubmitted" {"prompt" "change things"})
      (e "10:00:01.100" "user.message" {"content" "change things"} {"id" "u1"})]
     (call "10:00:02.000" "10:00:02.100" "t1" "edit" {"path" (str proj "/a.txt") "old_str" "one" "new_str" "two"} {"content" "ok"})
     (call "10:00:03.000" "10:00:03.100" "t2" "bash" {"command" (str "cd " proj " && cat > c.txt <<'EOF'\nnew file\nEOF")} {"content" ""})
     (call "10:00:04.000" "10:00:04.100" "t3" "bash" {"command" (str "cd " proj " && python3 - <<'EOF'\np='b.js'\ns=open(p).read()\nopen(p,'w').write(s.replace('1','2'))\nEOF")} {"content" ""})
     (call "10:00:05.000" "10:00:05.100" "t4" "apply_patch"
           "*** Begin Patch\n*** Add File: d.txt\n+d\n*** Update File: a.txt\n@@\n-two\n+three\n*** End Patch" {"content" "Applied"})
     (call "10:00:06.000" "10:00:06.100" "t5" "edit" {"path" (str proj "/a.txt") "old_str" "nowhere" "new_str" "x"} {"content" "no match"} false)
     [(said "10:00:07.000" "done")
      (hook "10:00:07.100" "agentStop" {"stopReason" "end_turn"})
      (hook "10:00:07.200" "sessionEnd" {"reason" "complete"})
      (hook "10:05:00.000" "userPromptSubmitted" {"prompt" "bump b"})
      (e "10:05:00.100" "user.message" {"content" "bump b"} {"id" "u2"})]
     (call "10:05:01.000" "10:05:01.100" "t6" "edit" {"path" (str proj "/b.js") "old_str" "x = 2" "new_str" "x = 3"} {"content" "ok"})
     [(said "10:05:02.000" "bumped")
      (hook "10:05:02.100" "agentStop" {"stopReason" "end_turn"})
      (hook "10:05:02.200" "sessionEnd" {"reason" "complete"})])))

(defn- fixture!
  "The project (two files, committed before the session), the Copilot home with the log and its rewind snapshots."
  []
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj"))
        dir (fs/path scratch "home" "session-state" sid)
        backups (fs/path dir "rewind-file-snapshots" "backups")
        backup! (fn [content] (let [h (cr/sha256 content)] (spit (str (fs/path backups h)) content) h))
        pre (fn [path content] {"path" path "preimage" (if content {"kind" "content" "backupFile" (backup! content)} {"kind" "absent"})})
        post (fn [m content] (assoc m "postimage" {"kind" "content" "contentHash" (cr/sha256 content)}))]
    (fs/create-dirs proj) (fs/create-dirs backups)
    (sh proj {} "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.txt")) "one\n")
    (spit (str (fs/path proj "b.js")) "x = 1\n")
    (sh proj {} "git" "add" ".")
    (sh proj {"GIT_AUTHOR_DATE" "2026-01-01T09:00:00Z" "GIT_COMMITTER_DATE" "2026-01-01T09:00:00Z"}
        "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "start")
    (spit (str (fs/path dir "events.jsonl")) (str/join "\n" (map json/generate-string (log-entries proj))))
    (spit (str (fs/path dir "workspace.yaml")) (str "id: " sid "\ncwd: " proj "\nbranch: main\nclient_name: github/cli\n"))
    ;; prompt 00: b.js was changed by the script, its new content only in prompt 01's backup (which is named by its hash)
    (let [s1 {"eventId" "u1" "timestamp" "2026-01-01T10:00:08.000Z"
              "files" {"1" (post (pre (str proj "/a.txt") "one\n") "three\n")
                       "2" (post (pre (str proj "/c.txt") nil) "new file\n")
                       "3" (post (pre (str proj "/b.js") "x = 1\n") "x = 2\n")
                       "4" (post (pre (str proj "/d.txt") nil) "d\n")}}
          s2 {"eventId" "u2" "timestamp" "2026-01-01T10:05:03.000Z"
              "files" {"1" (post (pre (str proj "/b.js") "x = 2\n") "x = 3\n")}}]
      (spit (str (fs/path dir "rewind-file-snapshots" "index.json")) (json/generate-string {"schema" 1 "snapshots" [s1 s2]})))
    {:proj proj :log (str (fs/path dir "events.jsonl")) :repo (str (fs/path scratch "replay"))}))

(deftest replay-a-session
  (let [{:keys [log repo]} (fixture!)
        st (cr/replay! [log] repo {})
        b "proj/s/5e551011"
        subj (str/split-lines (binding [g/*repo* repo] (git "log" "--reverse" "--first-parent" "--format=%s" b)))
        show (fn [rev path] (binding [g/*repo* repo] (git "show" (str rev ":" path))))
        changed (fn [rev] (binding [g/*repo* repo] (set (remove #(or (str/blank? %) (str/starts-with? % ".claude-trace/")) (str/split-lines (git "diff-tree" "-r" "--name-only" "--no-commit-id" rev))))))
        by-subject (binding [g/*repo* repo] (into {} (map #(vec (reverse (str/split % #"\t" 2))) (str/split-lines (git "log" "--first-parent" "--format=%H%x09%s" b)))))
        at (fn [s] (some (fn [[subject sha]] (when (str/starts-with? subject s) sha)) by-subject))]
    (testing "the commits"
      (is (= ["start" "prompt 00: change things" "00.1 Edit: a.txt" "00.2 Bash: cd" "00.3 Bash: cd" "00.4 Edit: d.txt"
              "00.5 Edit: a.txt" "response 00" "prompt 01: bump b" "01.1 Edit: b.js" "response 01"]
             (map #(str/replace % #"^(\d\d\.\d Bash: cd).*" "$1") (remove #(str/starts-with? % "trace of") subj)))))
    (testing "each call's change is its own diff"
      (is (= #{"a.txt"} (changed (at "00.1 "))))
      (is (= "two" (show (at "00.1 ") "a.txt")))
      (is (= #{"c.txt"} (changed (at "00.2 "))) "the here-document")
      (is (= "new file" (show (at "00.2 ") "c.txt")))
      (is (= #{"b.js"} (changed (at "00.3 "))) "the script's change, found under its hash and put at the last call naming b.js")
      (is (= "x = 2" (show (at "00.3 ") "b.js")))
      (is (= #{"a.txt" "d.txt"} (changed (at "00.4 "))) "apply_patch")
      (is (= ["three" "d"] [(show (at "00.4 ") "a.txt") (show (at "00.4 ") "d.txt")]))
      (is (= #{} (changed (at "00.5 "))) "a failed edit changes nothing")
      (is (= "x = 3" (show (at "01.1 ") "b.js"))))
    (testing "the numbers"
      (is (= {:ok 5} (select-keys (:post st) [:ok :bad])) "every file right at the end of its prompt, by Copilot's own hash")
      (is (= 1 (:filled st)))
      (is (nil? (:write-misses st)) "a failed edit is not looked at"))))

(deftest an-edit-whose-text-is-not-there-is-a-miss
  (is (= [[] 1] (cr/written "/p" "edit" {"path" "/p/a" "old_str" "zz" "new_str" "y"} {"/p/a" "abc"})))
  (is (= [[["/p/a" "ayc"]] 0] (cr/written "/p" "edit" {"path" "/p/a" "old_str" "b" "new_str" "y"} {"/p/a" "abc"})))
  (is (= [[["/p/n.txt" "1\n"] ["/p/n.txt" "1\n2\n"]] 0]
         (cr/written "/p" "bash" {"command" "cat > n.txt <<'E'\n1\nE\ncat >> n.txt <<'E'\n2\nE"} {}))
      "an append adds to what the same command wrote before"))

(deftest a-repo-inside-a-repo-is-its-own-project
  (let [base (str (fs/path scratch "nested"))
        outer (str (fs/path base "outer")) inner (str (fs/path outer ".scratch" "run"))
        commit! (fn [dir date]
                  (sh dir {} "git" "init" "-q" "-b" "main")
                  (spit (str (fs/path dir "f.txt")) dir)
                  (sh dir {} "git" "add" ".")
                  (sh dir {"GIT_AUTHOR_DATE" date "GIT_COMMITTER_DATE" date} "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "c"))]
    (fs/delete-tree base) (fs/create-dirs inner)
    (spit (str (fs/path outer ".gitignore")) ".scratch/\n")
    (commit! outer "2026-01-01T09:00:00Z")
    (commit! inner "2026-01-02T09:00:00Z")
    (let [sh* (shadow/create {})
          _ (shadow/open! sh* outer)
          _ (shadow/open! sh* inner)
          d (shadow/disk sh* (str (fs/path base "traces")))]
      (try
        (is (= "run" (:project (disk/project-of d (str inner "/x")))) "opened after the repo around it, still its own")
        (is (= "outer" (:project (disk/project-of d (str outer "/y")))))
        (is (= :first (shadow/sync! sh* (str inner "/f.txt") (.toEpochMilli (java.time.Instant/parse "2026-01-01T12:00:00Z")) nil))
            "no commit that old: the oldest one, not today's disk")
        (is (= inner (shadow/content sh* (str inner "/f.txt"))))
        (finally (shadow/close! sh*))))))
