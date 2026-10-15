(ns bench.peek
  "Print the head of the eval-style calls of both sessions (what they evaluate and why)."
  (:require [bench.env :as env] [clojure.edn :as edn] [clojure.string :as str]))

(defn- cs [s] (map edn/read-string (str/split-lines (slurp (env/calls-file s)))))
(defn head [s n] (let [s (str/replace s "\n" " ")] (subs s 0 (min n (count s)))))

(defn -main [& args]
  (println "SANE cljdev eval, non-restart:")
  (doseq [c (cs "sane") :let [k (or (get-in c [:input :command]) "")] :when (and (re-find #"cljdev eval" k) (not (re-find #"c/stop" k)))]
    (println (:round c) (head (str/replace k #"^.*?cljdev eval --dir \S+ " "") 150)))
  (println "\nLAME clojure_eval:")
  (doseq [c (cs "lame") :when (= (:tool c) "mcp__clj-um-lame__clojure_eval")]
    (println (:round c) (head (pr-str (:input c)) 150)))
  (println "\nLAME bash with cljdev:")
  (doseq [c (cs "lame") :let [k (or (get-in c [:input :command]) "")] :when (re-find #"cljdev" k)]
    (println (:round c) (head k 110))))
