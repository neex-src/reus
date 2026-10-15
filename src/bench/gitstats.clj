(ns bench.gitstats
  "Per-session stats from the bench trace history (<s>/um-<s>): commits by kind, failures, most-edited files, reads per file."
  (:require [babashka.process :as p] [bench.env :as env] [clojure.string :as str]))

(defn git [dir & a] (str/split-lines (:out (apply p/shell {:out :string :dir dir} "git" a))))

(defn -main [& args]
  (doseq [s env/sessions]
    (let [dir (env/checkout s)
          shas (git dir "log" "--no-merges" "--format=%H" "snapshot..HEAD")
          info (for [sha shas]
                 (let [files (remove str/blank? (git dir "diff-tree" "--no-commit-id" "--name-only" "-r" sha))
                       subj (first (git dir "log" "-1" "--format=%s" sha))
                       body (str/join "\n" (git dir "log" "-1" "--format=%b" sha))
                       real (remove #(or (= % "doto.md") (str/starts-with? % "reads/")) files)
                       kind (cond (str/starts-with? subj "prompt ") :prompt (str/starts-with? subj "response ") :response
                                  (seq real) :edit (re-matches #"[0-9a-f]{40}" subj) :read-file :else :read-cmd)]
                   {:kind kind :real real :err? (str/includes? body "--- error ---")
                    :readpath (when (= kind :read-file) (first files)) :subj subj}))
          by (group-by :kind info)]
      (println "\n==" s (count info) "commits")
      (doseq [[k v] (sort-by key by)] (printf "  %-10s %3d\n" (name k) (count v)))
      (println "  edit commits with error text:" (count (filter :err? (:edit by))))
      (println "  most-edited files:" (->> (mapcat :real (:edit by)) frequencies (sort-by (comp - val)) (take 6) (map (fn [[f n]] (str (last (str/split f #"/")) "x" n))) (str/join " ")))
      (println "  most-read files  :" (->> (:read-file by) (keep :readpath) frequencies (sort-by (comp - val)) (take 6) (map (fn [[f n]] (str (last (str/split f #"/")) "x" n))) (str/join " ")))
      (println "  edits touching >1 file:" (count (filter #(> (count (:real %)) 1) (:edit by))) "of" (count (:edit by))))))
