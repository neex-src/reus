(ns trace.git
  "Git access for the trace tools: which repo, running git in it, reading many
  objects in one process."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(def default-repo (str (fs/path (fs/home) "projects" "reus" "var" "traces")))

(def ^:dynamic *repo*
  "The trace repo. $TRACE_REPO, else ~/projects/reus/var/traces."
  (or (System/getenv "TRACE_REPO") default-repo))

(def ^:dynamic *env*
  "Environment added to every git call (a replay sets GIT_AUTHOR_DATE / GIT_COMMITTER_DATE)."
  {})

(def ^:private ident ["-c" "user.name=reus-trace" "-c" "user.email=trace@reus.local"])

(defn git*
  "Run git in *repo* with process opts (:in, :extra-env, :dir ...); the full result."
  [opts & args]
  (apply p/shell (merge {:out :string :err :string :continue true :dir *repo*}
                        opts {:extra-env (merge *env* (:extra-env opts))})
         "git" (concat ident args)))

(defn git
  "Run git in *repo*; trimmed stdout. A trailing map is process opts."
  [& args]
  (let [[opts args] (if (map? (last args)) [(last args) (butlast args)] [{} args])]
    (str/trim-newline (:out (apply git* opts args)))))

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
