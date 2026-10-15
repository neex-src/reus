(ns bench.extract
  "Flatten each session transcript into analysis/<s>-calls.edn: one map per tool call
  with round (prompt index), tool, input, error? and the head of the result."
  (:require [babashka.fs :as fs]
            [bench.env :as env]
            [cheshire.core :as j]
            [clojure.java.io :as io]))

(defn- blocks [m] (let [c (get-in m [:message :content])] (if (sequential? c) c [])))

(defn -main [& _]
  (doseq [s env/sessions]
    (let [ms (map #(j/parse-string % true) (line-seq (io/reader (env/transcript s))))
          round (atom -1) calls (atom []) idx (atom {})]
      (doseq [m ms]
        (case (:type m)
          "user" (let [c (get-in m [:message :content])]
                   (if (string? c)
                     (swap! round inc)
                     (doseq [b (blocks m)
                             :when (= "tool_result" (:type b))]
                       (when-let [i (@idx (:tool_use_id b))]
                         (let [r (:content b) txt (if (string? r) r (apply str (map :text r)))]
                           (swap! calls assoc-in [i :error] (boolean (:is_error b)))
                           (swap! calls assoc-in [i :result] (subs txt 0 (min 300 (count txt)))))))))
          "assistant" (doseq [b (blocks m) :when (= "tool_use" (:type b))]
                        (swap! idx assoc (:id b) (count @calls))
                        (swap! calls conj {:round @round :tool (:name b) :input (:input b) :ts (:timestamp m)}))
          nil))
      (fs/create-dirs (fs/parent (env/calls-file s)))
      (spit (env/calls-file s) (with-out-str (doseq [c @calls] (prn c))))
      (println s (count @calls) "calls, rounds" (inc @round)))))
