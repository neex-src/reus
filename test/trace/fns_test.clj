(ns trace.fns-test
  "trace.fns over a trace that trace.record makes of a throwaway project under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.ash :as ash]
            [trace.fns :as fns]
            [trace.git :as g :refer [git]]
            [trace.record :as rec]))

(defmacro deftest-ash
  "A deftest of the fn layer: skipped where ashamer is absent (the sandbox), where only the layer needs it."
  [nm & body]
  `(deftest ~nm (if (ash/available?) (do ~@body) (println "skipped" '~nm ": ashamer is absent"))))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "fns-test")))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))
(defn- real [dir & args] (str/trim (:out (apply sh dir "git" "-c" "user.name=t" "-c" "user.email=t@t" args))))
(defn- ev [event m] (rec/handle! event m))
(defn- log [& args] (vec (remove str/blank? (str/split-lines (apply git "log" args)))))
(defn- parents [rev] (str/split (git "log" "-1" "--format=%P" rev) #" "))

(deftest-ash explode
  (is (= {"ns" "(ns a\n  (:require [x]))\n" "f" "(defn ^:private f [] 1)\n" "x" "(def x 1)\n" "x~2" "(def x 2)\n"
          "area :circle" "(defmethod area :circle [c] 3)\n" "::k" "(s/def ::k int?)\n" "a%2Fb" "(defn a/b [])\n"}
         (fns/explode (str "(ns a\n  (:require [x]))\n\n;; c\n(defn ^:private f [] 1)\n(def x 1)\n(def x 2)\n"
                           "(defmethod area :circle [c] 3)\n(comment (defn g []))\n(s/def ::k int?)\n(defn a/b [])\n")))
      "definitions by name, a second def numbered, other forms left out")
  (is (= {"ns" "(ns a)\n" "f" "(defn f [] (\n" "g" "(defn ^:private g [] 2)\n"}
         (fns/explode "(ns a)\n\n(defn f [] (\n\n;; trailing\n(defn ^:private g [] 2)\n"))
      "a file that does not parse is cut at lines that start with ("))

(defn- fn-refs [] (sort (str/split-lines (git "for-each-ref" "--format=%(refname) %(objectname)" "refs/fn/"))))

(deftest-ash fn-layer
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj")) wt (str (fs/path scratch "wt")) a (str (fs/path proj "a.clj"))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit a "(ns a)\n\n(defn f [] 1)\n")
    (spit (str (fs/path proj "notes.txt")) "not clojure\n")
    (sh proj "git" "add" ".")
    (real proj "commit" "-q" "-m" "init")
    (real proj "worktree" "add" "-q" "../wt" "-b" "side")
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "change f"})
      (spit a "(ns a)\n\n(defn f [] 2)\n")
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Edit" "tool_input" {"file_path" a} "tool_response" "ok"})
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Read" "tool_input" {"file_path" a} "tool_response" "x"})
      (spit a "(ns a)\n\n(defn f [] 2)\n\n(defn g [] 3)\n")
      (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Edit" "tool_input" {"file_path" a} "tool_response" "ok"})
      (ev "stop" {"session_id" "s1" "last_assistant_message" "done"})
      (ev "prompt" {"session_id" "s2" "cwd" wt "prompt" "add h"})
      (spit (str (fs/path wt "b.clj")) "(ns b)\n\n(defn h [] :h)\n")
      (real wt "add" ".")
      (real wt "commit" "-q" "-m" "b: h")
      (ev "tool" {"session_id" "s2" "cwd" wt "tool_name" "Bash" "tool_input" {"command" "git commit"} "tool_response" {"stdout" "ok"}})
      (ev "stop" {"session_id" "s2" "last_assistant_message" "done"})
      (real proj "merge" "-q" "--no-ff" "side" "-m" "Merge side")
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "look"})
      (let [r (fns/run! g/*repo*)]
        (testing "a fn ref beside every trace branch, made through the live store"
          (is (= ["refs/fn/proj/main" "refs/fn/proj/s/s1/proj" "refs/fn/proj/s/s2/wt"] (map #(first (str/split % #" ")) (fn-refs))))
          (is (< 0 (:commits r) fns/batch-over))))
      (testing "the tree: a dir of definitions per Clojure file, nothing else"
        (is (= ["a.clj/f" "a.clj/g" "a.clj/ns"] (str/split-lines (git "ls-tree" "-r" "--name-only" "fn/proj/s/s1/proj~1"))))
        (is (= "(defn f [] 2)" (git "show" "fn/proj/s/s1/proj~1:a.clj/f"))))
      (testing "the first-parent line is the fn history, a commit only where a definition changed"
        (is (= ["fn b.clj: +h, +ns" "fn a.clj: +g" "fn a.clj: f" "fn a.clj: +f, +ns" "fn layer of proj"] (log "--first-parent" "--format=%s" "fn/proj/s/s1/proj"))
            "the merged b.clj comes in with s1's next prompt, whose tree is the main checkout's after the merge")
        (is (= ["fn a.clj: f" "fn a.clj: +f, +ns"] (log "--first-parent" "--format=%s" "fn/proj/s/s1/proj" "--" "a.clj/f"))))
      (testing "parent 2 is the trace commit: the call that changed it, with its prompt and worktree"
        (let [[p1 ctx :as ps] (parents "fn/proj/s/s1/proj~2")]
          (is (= 2 (count ps)))
          (is (= (git "rev-parse" "fn/proj/s/s1/proj~3") p1))
          (is (str/starts-with? (git "log" "-1" "--format=%s" ctx) "00.1 Edit"))
          (is (= "0" (str (:exit (g/git* {} "merge-base" "--is-ancestor" ctx "proj/s/s1")))))
          (is (= "(ns a)\n\n(defn f [] 2)" (git "show" (str ctx ":a.clj"))))
          (is (= (git "log" "-1" "--format=%an %ad" ctx) (git "log" "-1" "--format=%an %ad" "fn/proj/s/s1/proj~2")) "the trace commit's idents")
          (is (str/includes? (git "log" "-1" "--format=%B" "fn/proj/s/s1/proj~2") "Fn-Changed: a.clj/f"))))
      (testing "a session that changed nothing points at the fn state it started from; s2's line starts at main's"
        (is (= (git "rev-parse" "fn/proj/s/s2/wt~1") (git "rev-parse" "fn/proj/main~1")))
        (is (= ["a.clj/f" "a.clj/ns" "b.clj/h" "b.clj/ns"] (str/split-lines (git "ls-tree" "-r" "--name-only" "fn/proj/s/s2/wt")))))
      (testing "the mirrored real merge: its fn commit has the merged session's fn tip as a further parent"
        (let [[p1 ctx s2] (parents "fn/proj/main")]
          (is (= (git "rev-parse" "fn/proj/main~1") p1))
          (is (= (git "rev-parse" "proj/main") ctx))
          (is (= (git "rev-parse" "fn/proj/s/s2/wt") s2)))
        (is (= ["fn b.clj: +h, +ns"] (log "--format=%s" "fn/proj/main" "--" "b.clj/h"))
            "a fn's log through main goes into the session that wrote it")
        (is (= (git "rev-parse" "fn/proj/s/s2/wt") (first (log "--format=%H" "fn/proj/main" "--" "b.clj/h")))))
      (testing "incremental: the next run looks at the new trace commits only"
        (spit a "(ns a)\n\n(defn f [] 2)\n\n(defn g [] (\n")
        (ev "tool" {"session_id" "s1" "cwd" proj "tool_name" "Edit" "tool_input" {"file_path" a} "tool_response" "ok"})
        (let [before (git "rev-parse" "fn/proj/s/s1/proj")
              r (fns/run! g/*repo*)]
          (is (= {:commits 1 :made 1} r))
          (is (= "fn a.clj: g" (git "log" "-1" "--format=%s" "fn/proj/s/s1/proj")) "a file that does not parse changes only what changed")
          (is (= before (first (parents "fn/proj/s/s1/proj"))))
          (is (= {:commits 0 :made 0} (fns/run! g/*repo*)))))
      (testing "made again from scratch, live or through fast-import, it has the same shas"
        (let [want (fn-refs)
              wipe! #(do (doseq [l want] (g/git* {} "update-ref" "-d" (first (str/split l #" "))))
                         (fs/delete-tree (fs/path g/*repo* ".git" "trace-fns")))]
          (wipe!)
          (fns/run! g/*repo*)
          (is (= want (fn-refs)) "live")
          (wipe!)
          (with-redefs [fns/batch-over 0] (fns/run! g/*repo*))
          (is (= want (fn-refs)) "fast-import")
          (is (zero? (:exit (g/git* {} "fsck" "--connectivity-only" "--no-dangling")))))))))

(deftest-ash a-line-per-worktree
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj")) wt (str (fs/path scratch "wt")) b (str (fs/path wt "b.clj"))
        call (fn [cwd tool input] (ev "tool" {"session_id" "s3" "cwd" cwd "tool_name" tool "tool_input" input "tool_response" "ok"}))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.clj")) "(ns a)\n\n(defn f [] 1)\n")
    (sh proj "git" "add" ".")
    (real proj "commit" "-q" "-m" "init")
    (real proj "worktree" "add" "-q" "../wt" "-b" "side")
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (ev "prompt" {"session_id" "s3" "cwd" wt "prompt" "b in the worktree, looking at the main checkout now and then"})
      (spit b "(ns b)\n\n(defn h [] 1)\n")
      (call wt "Write" {"file_path" b})
      (call proj "Bash" {"command" "touch notes.txt"})
      (spit b "(ns b)\n\n(defn h [] 2)\n")
      (call wt "Edit" {"file_path" b})
      (call proj "Bash" {"command" "rm notes.txt"})
      (spit b "(ns b)\n\n(defn h [] 2)\n\n(defn k [] 3)\n")
      (call wt "Edit" {"file_path" b})
      (real wt "add" ".")
      (real wt "commit" "-q" "-m" "b")
      (call wt "Bash" {"command" "git commit -m b"})
      (real proj "merge" "-q" "--no-ff" "side" "-m" "Merge side")
      (call proj "Bash" {"command" "git merge --no-ff side"})
      (ev "stop" {"session_id" "s3" "last_assistant_message" "merged"})
      (fns/run! g/*repo*)
      (testing "a fn ref per worktree the session worked in"
        (is (= ["refs/fn/proj/main" "refs/fn/proj/s/s3/proj" "refs/fn/proj/s/s3/wt"] (map #(first (str/split % #" ")) (fn-refs)))))
      (testing "the worktree's line has its own history: the calls in the main checkout remove nothing from it"
        (is (= ["fn b.clj: +k" "fn b.clj: h" "fn b.clj: +h, +ns" "fn a.clj: +f, +ns" "fn layer of proj"]
               (log "--first-parent" "--format=%s" "fn/proj/s/s3/wt")))
        (is (not-any? #(re-find #"-h|-ns" %) (log "--format=%s" "fn/proj/s/s3/wt" "fn/proj/s/s3/proj" "fn/proj/main" "--not" "proj/main"))))
      (testing "the merge in the main checkout came from the worktree's line: a fn's log goes there"
        (is (= "fn b.clj: +h, +k, +ns" (git "log" "-1" "--format=%s" "fn/proj/s/s3/proj")))
        (is (= (git "rev-parse" "fn/proj/s/s3/wt") (nth (parents "fn/proj/s/s3/proj") 2)))
        (is (= ["fn b.clj: h" "fn b.clj: +h, +ns"] (log "--format=%s" "fn/proj/s/s3/proj" "--" "b.clj/h"))))
      (testing "and so does main's, through the mirrored merge"
        (is (= ["fn b.clj: h" "fn b.clj: +h, +ns"] (log "--format=%s" "fn/proj/main" "--" "b.clj/h")))))))
