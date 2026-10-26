(ns trace.git
  "Git access for the trace tools: which repo, running git in it, reading many
  objects in one process."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def home
  "The reus checkout this code runs from: the dir of the bb.edn it was started with, else the one above
  src/. What reus keeps by default is found from here, so a checkout behaves the same wherever it lives
  (~/projects/reus on the host, ~/projects/reus in the sandbox)."
  (str (fs/canonicalize
        (or (some-> (System/getProperty "babashka.config") fs/parent)
            (some-> (io/resource "trace/git.clj") io/file fs/parent fs/parent fs/parent)))))

(def default-repo (str (fs/path home "var" "traces")))

(def ^:dynamic *repo*
  "The trace repo. $TRACE_REPO, else <home>/var/traces."
  (or (System/getenv "TRACE_REPO") default-repo))

(def ^:dynamic *env*
  "Environment added to every git call (a replay sets GIT_AUTHOR_DATE / GIT_COMMITTER_DATE)."
  {})

(def ^:private ident ["-c" "user.name=reus-trace" "-c" "user.email=trace@reus.local"])

(defn git*
  "Run git in *repo* with process opts (:in, :extra-env, :dir ...); the full result, whatever the exit."
  [opts & args]
  (apply p/shell (merge {:out :string :err :string :continue true :dir *repo*}
                        opts {:extra-env (merge *env* (:extra-env opts))})
         "git" (concat ident args)))

(defn- opts+args [args] (if (map? (last args)) [(last args) (butlast args)] [{} args]))

(defn git
  "Run git in *repo*; trimmed stdout (empty when it fails: for readers). A trailing map is process opts."
  [& args]
  (let [[opts args] (opts+args args)]
    (str/trim-newline (:out (apply git* opts args)))))

(defn git!
  "git for writes: throws when git fails, so a write that did not happen never looks like one that did."
  [& args]
  (let [[opts args] (opts+args args)
        r (apply git* opts args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (str "git " (first args) " failed: " (str/trim (str (:err r))))
                      {:args (vec args) :exit (:exit r) :err (:err r)})))
    (str/trim-newline (:out r))))

(defn cat-batch
  "{rev content} for every rev git can resolve, in one cat-file process."
  [revs]
  (if (empty? revs)
    {}
    (let [^bytes bs (:out (p/shell {:out :bytes :err :string :continue true :dir *repo*
                                    :in (str (str/join "\n" revs) "\n")}
                                   "git" "cat-file" "--batch"))
          n (alength bs)]
      (loop [i 0 [r & more :as left] revs acc {}]
        (if (or (empty? left) (>= i n))
          acc
          (let [nl (loop [j i] (if (or (>= j n) (= 10 (aget bs j))) j (recur (inc j))))
                header (String. bs i (- nl i) "UTF-8")
                [_ _ size] (str/split header #" ")]
            (if-let [size (and (not (str/ends-with? header "missing")) (parse-long (or size "")))]
              (recur (+ nl 1 size 1) more (assoc acc r (String. bs (inc nl) (int size) "UTF-8")))
              (recur (inc nl) more acc))))))))

(defn have
  "The revs among revs that git can resolve (a blob sha, <commit>:<path>), in one cat-file process."
  [revs]
  (if (empty? revs)
    #{}
    (let [out (:out (p/shell {:out :string :err :string :continue true :dir *repo*
                              :in (str (str/join "\n" revs) "\n")}
                             "git" "cat-file" "--batch-check"))]
      (set (keep (fn [[r l]] (when-not (str/ends-with? l " missing") r))
                 (map vector revs (str/split-lines out)))))))
