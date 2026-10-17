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

(defn- script
  "The same operations on a store; the ids it made."
  [s]
  (store/ensure! s)
  (let [root (store/commit! s {:parents [] :base nil :files {} :subject "trace of x" :body "root /x"})
        _ (store/advance! s "refs/heads/x" root)
        p (store/commit! s {:parents [root] :base root :files {"doto.md" "do it"} :subject "prompt 00: do it" :body "session s1\ncwd /x"})
        t (store/commit! s {:parents [p] :base p :files {"reads/a.clj" "(ns a)\n" "files/b" (byte-array [0 1 2 255])}
                            :subject (store/blob! s "(ns a)\n") :body "cat a.clj\n"})
        r (store/commit! s {:parents [t] :base t :files {"doto.md" "done"} :subject "response 00" :body ""})
        m (store/commit! s {:parents [root r] :base r :files {} :subject "prompt 00: do it"})]
    (store/advance! s "refs/heads/x" m)
    (store/advance! s "refs/heads/x-p00" r)
    (store/note! s "refs/notes/event" t "{\"raw\":1}")
    (store/kv! s :sessions "s1" {:k 1 :merge ["a"]})
    (store/kv! s :prev "s1" "toolu_1")
    (store/kv! s :last "s2" {:a 1})
    (store/kv! s :last "s2" nil)
    {:has [(store/has? s t "reads/a.clj") (store/has? s root "reads/a.clj") (store/has? s m "reads/a.clj")]
     :tips [(store/tip s "refs/heads/x") (store/tip s "refs/heads/nope")]
     :refs (sort (store/refs s "refs/heads/x"))
     :kv [(store/kv s :sessions "s1") (store/kv s :prev "s1") (store/kv s :last "s2") (store/kv-all s :sessions)]}))

(deftest live-and-batch-agree
  (fs/delete-tree scratch)
  (let [live-repo (str (fs/path scratch "live")) batch-repo (str (fs/path scratch "batch"))
        l (binding [g/*env* env] (script (store/live live-repo)))
        b (let [s (store/batch batch-repo)]
            (store/at! s 1767265200000)
            (let [r (script s)] (store/close! s) r))]
    (testing "the store answers alike"
      (is (= (dissoc l :tips :refs) (dissoc b :tips :refs)))
      (is (= [true false true] (:has b)))
      (is (nil? (second (:tips b)))))
    (testing "the repos hold the same history: refs, commits (so trees, messages, dates) and notes"
      (let [out (fn [repo & args] (:out (apply sh repo {} "git" args)))
            heads #(out % "for-each-ref" "--format=%(refname) %(objectname)" "refs/heads")]
        (is (= (heads live-repo) (heads batch-repo)))
        (is (= (out live-repo "log" "--all" "--format=raw" "--no-notes") (out batch-repo "log" "--all" "--format=raw" "--no-notes")))
        (is (= (out live-repo "notes" "--ref=event" "list") (out batch-repo "notes" "--ref=event" "list")))
        (is (= "{\"raw\":1}" (str/trim (out batch-repo "notes" "--ref=event" "show" (str/trim (out batch-repo "rev-parse" "refs/heads/x-p00~1"))))))))))

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
        (is (= {:project "proj" :root proj} (disk/project-of d proj)))
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
