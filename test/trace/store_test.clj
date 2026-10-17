(ns trace.store-test
  "trace.store (live and batch must agree) and trace.shadow (the disk of a past session)
  against throwaway repos under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.disk :as disk]
            [trace.git :as g :refer [git]]
            [trace.shadow :as shadow]
            [trace.store :as store]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "store-test")))

(defn- sh [dir env & args] (apply p/shell {:dir (str dir) :out :string :err :string :extra-env env} args))

(def date (store/git-date 1767265200000))
(def env {"GIT_AUTHOR_DATE" date "GIT_COMMITTER_DATE" date})

(def author "Ann <ann@example.org> 1767265200 +0100")
(def committer "Bob <bob@example.org> 1767268800 -0500")

(defn- script
  "The same operations on a store, over a base tree that both repos can read; what it answers."
  [s base]
  (store/ensure! s)
  (let [t1 (store/tree! s {"prompt.md" "do it" "memory/m.md" "x\n" "claude-md/c.md" (byte-array [0 1 2 255])})
        root (store/commit! s {:parents [] :tree {:base nil} :subject "trace of x" :body "root /x"})
        _ (store/advance! s "refs/heads/x/main" root)
        p (store/commit! s {:parents [root] :tree {:base base :subtrees {".claude-trace" t1}}
                            :subject "prompt 00: do it" :body "session s1\n\nCwd: /x"})
        t (store/commit! s {:parents [p] :tree {:base base :subtrees {".claude-trace" t1}
                                                :changes {"new.txt" "n\n" "a.clj" nil "d/deep.txt" "deep"}}
                            :subject "00.1 Bash: ls" :body "ls\n\nTool: Bash" :author author :committer committer})
        r (store/commit! s {:parents [t] :tree {:base base :subtrees {".claude-trace" (store/tree! s {"prompt.md" "do it" "reply.md" "done"})}}
                            :subject "response 00" :body ""})
        m (store/commit! s {:parents [root r] :tree {:base base} :subject "Merge side" :body "the real message\n\nReal-Commit: abc\n"
                            :author author :committer committer})]
    (store/advance! s "refs/heads/x/main" m)
    (store/advance! s "refs/heads/x/s/s1" r)
    (store/note! s "refs/notes/event" t "{\"raw\":1}")
    (store/note! s "refs/notes/output" t "out\n")
    (store/kv! s :sessions "s1" {:k 1 :merge ["a"]})
    (store/kv! s :prev "s1" "toolu_1")
    (store/kv! s :last "s2" {:a 1})
    (store/kv! s :last "s2" nil)
    {:tips [(some? (store/tip s "refs/heads/x/main")) (store/tip s "refs/heads/nope")]
     :refs (count (store/refs s "refs/heads/x/"))
     :shas (mapv #(store/sha-of s %) [root p t r m])
     :kv [(store/kv s :sessions "s1") (store/kv s :prev "s1") (store/kv s :last "s2") (store/kv-all s :sessions)]}))

(deftest live-and-batch-agree
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj")) live-repo (str (fs/path scratch "live")) batch-repo (str (fs/path scratch "batch"))]
    (fs/create-dirs proj)
    (sh proj {} "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.clj")) "(ns a)\n")
    (spit (str (fs/path proj "z.txt")) "z\n")
    (sh proj {} "git" "add" ".")
    (sh proj {} "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (let [base (str/trim (:out (sh proj {} "git" "rev-parse" "HEAD^{tree}")))
          objects (str (fs/path proj ".git" "objects"))
          l (binding [g/*env* env]
              (let [s (store/live live-repo)] (store/ensure! s) (store/alternate! s objects) (script s base)))
          b (let [s (store/batch batch-repo {:alternates [objects]})]
              (store/at! s 1767265200000)
              (let [r (script s base)] (store/close! s) r))]
      (testing "the store answers alike, a batch's commit ids resolve to the live shas"
        (is (= l b))
        (is (= 5 (count (distinct (:shas b))))))
      (testing "the repos hold the same history: refs, commits (so trees, messages, idents) and notes"
        (let [out (fn [repo & args] (:out (apply sh repo {} "git" args)))
              heads #(out % "for-each-ref" "--format=%(refname) %(objectname)" "refs/heads")]
          (is (= (heads live-repo) (heads batch-repo)))
          (is (= (out live-repo "log" "--all" "--format=raw" "--no-notes") (out batch-repo "log" "--all" "--format=raw" "--no-notes")))
          (is (= (out live-repo "notes" "--ref=event" "list") (out batch-repo "notes" "--ref=event" "list")))
          (is (= (out live-repo "notes" "--ref=output" "list") (out batch-repo "notes" "--ref=output" "list")))
          (testing "a tree: the base, with the subtree in, files set and one removed"
            (let [files (str/split-lines (out batch-repo "ls-tree" "-r" "--name-only" "x/s/s1~1"))]
              (is (= [".claude-trace/claude-md/c.md" ".claude-trace/memory/m.md" ".claude-trace/prompt.md" "d/deep.txt" "new.txt" "z.txt"]
                     (sort (str/split-lines (out batch-repo "ls-tree" "-r" "--name-only" "refs/heads/x/s/s1~1"))))
                  (str "got " (pr-str files))))))))))

(deftest blob-sha-is-gits
  (is (= (git "hash-object" "--stdin" {:in "hello\n" :dir "."}) (store/blob-sha "hello\n")))
  (is (= "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" (store/blob-sha ""))))

;; --- shadow ---------------------------------------------------------------------------

(defn- commit! [proj t files msg]
  (doseq [[f c] files] (spit (str (fs/path proj f)) c))
  (sh proj {} "git" "add" ".")
  (let [d (str (quot t 1000) " +0000")]
    (sh proj {"GIT_AUTHOR_DATE" d "GIT_COMMITTER_DATE" d} "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" msg)))

(deftest shadow-keeps-the-overlay-like-a-checkout
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj"))
        a (str proj "/a.clj") same (str proj "/same.clj") u (str proj "/untracked.txt")
        t1 1767268800000 t2 1767276000000]
    (fs/create-dirs proj)
    (sh proj {} "git" "init" "-q" "-b" "main")
    (commit! proj t1 {"a.clj" "v1\n" "same.clj" "s\n"} "one")
    (commit! proj t2 {"a.clj" "v2\n"} "two")
    (let [sh* (shadow/create {})
          d (shadow/disk sh* (str scratch "/trace"))]
      (try
        (is (= :reflog (shadow/sync! sh* a t1 nil)))
        (is (= "v1\n" (shadow/content sh* a)))
        (is (nil? (shadow/content sh* u)))
        (shadow/write! sh* same "mine\n")
        (shadow/write! sh* u "new\n")
        (is (= "s\n" (disk/head-content d proj "same.clj")) "HEAD is the commit, not the working tree")
        (is (= "mine\n" (String. ^bytes (disk/file-bytes d same))))
        (is (= a (disk/resolve-file d "a.clj" [proj])) "a token names the file of the commit")
        (is (nil? (disk/resolve-file d "nope.clj" [proj])))
        (is (= {:project "proj" :root proj} (select-keys (disk/project-of d proj) [:project :root])))
        (testing "to the next commit: changes in files the commits share are carried along, as git does"
          (shadow/sync! sh* a t2 nil)
          (is (= "v2\n" (shadow/content sh* a)))
          (is (= "mine\n" (shadow/content sh* same)))
          (is (= "new\n" (shadow/content sh* u))))
        (testing "back: a changed file that differs between the commits makes git refuse, so reset --hard; untracked stays"
          (shadow/write! sh* a "edited\n")
          (shadow/sync! sh* a t1 nil)
          (is (= "v1\n" (shadow/content sh* a)))
          (is (= "new\n" (shadow/content sh* u)))
          (is (= "s\n" (shadow/content sh* same)) "tracked changes go with a reset"))
        (testing "a deleted file stays deleted"
          (shadow/delete! sh* same)
          (is (nil? (shadow/content sh* same)))
          (is (nil? (disk/resolve-file d "same.clj" [proj]))))
        (finally (shadow/close! sh*))))))
