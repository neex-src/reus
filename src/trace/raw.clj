(ns trace.raw
  "The trace as plain text, for a terminal or an agent.

  bb trace:raw                     projects: prompts, last activity
  bb trace:raw <project>           its prompts: when, steps by kind, the prompt
  bb trace:raw <project> <NN> [-v] one prompt, every step: command, error, and
                                   with -v also output and diff; then the reply
  $TRACE_REPO picks another trace repo (a bench one: <project> is lame, sane, ...)."
  (:require [clojure.string :as str]
            [trace.git :as g :refer [git cat-batch]]
            [trace.read :as r])
  (:import [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- stamp [secs]
  (if secs
    (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
             (.atZone (Instant/ofEpochSecond secs) (ZoneId/systemDefault)))
    "?"))

(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))
(defn- indent [s] (str/join "\n" (map #(str "    " %) (str/split-lines (str s)))))
(defn- kind [s] (if (:err s) "FAILED" (str/upper-case (name (:kind s)))))
(defn- strip-ansi [s] (str/replace (str s) #"\x1b\[[0-9;?]*[A-Za-z]" ""))

(defn- counts [steps]
  (let [f (frequencies (map #(if (:err %) :err (:kind %)) steps))]
    (format "%3d steps  %3d read %3d edit %3d run %3d failed" (count steps)
            (f :read 0) (f :edit 0) (f :run 0) (f :err 0))))

(defn- projects [tips]
  (doseq [m (r/mains tips) :let [ds (r/all-prompts tips m)]]
    (println (format "%-24s %3d prompts  last %s" m (count ds) (stamp (:t1 (last ds)))))))

(defn- prompts [tips m]
  (let [live (r/live-branches)]
    (doseq [d (r/all-prompts tips m)]
      (println (format "%s  %s  %s%s  %s" (:nn d) (stamp (:t0 d)) (counts (:steps d))
                       (if (live (str m "-p" (:nn d))) "  RUNNING" "") (clip (:prompt d) 80))))))

(defn- prompt [tips m nn verbose?]
  (let [d (r/prompt-data tips m nn)]
    (when-not d (println "no prompt" nn "in" m) (System/exit 1))
    (println (str "=== " m " prompt " nn "  " (stamp (:t0 d)) (when (:cwd d) (str "  in " (:cwd d)))))
    (println (indent (:prompt d)))
    (println (str "--- " (counts (:steps d))))
    (let [outs (when verbose? (cat-batch (keep #(when (:result %) (str (:sha %) ":" (:result %))) (:steps d))))]
      (doseq [s (:steps d)]
        (println)
        (println (format "#%d %s %s  +%ds%s%s" (:i s) (kind s) (or (:tool (:call s)) "")
                         (- (:at s) (:t0 d))
                         (if (= :edit (:kind s)) (format "  +%d -%d %s" (:add s) (:del s) (str/join "," (map :path (:files s)))) "")
                         (if (:blob s) (str "  blob " (:blob s) " " (:read-file s)) "")))
        (println (indent (:cmd s)))
        (when (string? (:err s)) (println "  error:") (println (indent (:err s))))
        (when verbose?
          (when-let [o (get outs (str (:sha s) ":" (:result s)))]
            (println "  output:") (println (indent (strip-ansi o))))
          (when (seq (:files s))
            (println "  diff:")
            (println (indent (git "show" "--format=" "--patch" (:sha s) "--" "." ":(exclude)reads" ":(exclude)doto.md")))))))
    (println)
    (println "=== reply")
    (println (indent (or (:reply d) "(none yet)")))))

(defn -main [& args]
  (let [verbose? (some #{"-v"} args)
        [m nn] (remove #{"-v"} args)
        tips (r/tips)]
    (cond
      nn (prompt tips m (if (= 1 (count nn)) (str "0" nn) nn) verbose?)
      m (prompts tips m)
      :else (projects tips))))
