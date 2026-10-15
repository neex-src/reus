(ns bench.stats
  "Per-session totals from the jsonl transcripts: tool calls by name, tokens, errors."
  (:require [bench.env :as env]
            [cheshire.core :as j]
            [clojure.java.io :as io]))

(defn- content [m] (let [c (get-in m [:message :content])] (if (sequential? c) c [])))

(defn -main [& _]
  (doseq [s env/sessions]
    (let [ms (map #(j/parse-string % true) (line-seq (io/reader (env/transcript s))))
          asst (filter #(= "assistant" (:type %)) ms)
          tools (frequencies (map :name (filter #(= "tool_use" (:type %)) (mapcat content asst))))
          usage (map #(get-in % [:message :usage]) asst)
          sm #(reduce + (map (fn [u] (or (get u %) 0)) usage))
          errs (count (filter #(and (= "tool_result" (:type %)) (:is_error %))
                              (mapcat content (filter #(= "user" (:type %)) ms))))]
      (println s "assistant-msgs" (count asst) "tool-calls" (reduce + (vals tools)) "tool-errors" errs
               "out-tok" (sm :output_tokens) "cache-read" (sm :cache_read_input_tokens) "in-tok" (sm :input_tokens))
      (println "  " (into (sorted-map) tools)))))
