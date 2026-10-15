(ns jsonl.peek
  "What the sessions evaluated in a REPL: `cljdev eval` from the shell and clojure-mcp's
  clojure_eval, one line each with the prompt number.

  bb jsonl:peek [sel] [--agents]"
  (:require [clojure.string :as str]
            [jsonl.read :as r]))

(defn- head [s n] (let [s (str/replace (str s) #"\s+" " ")] (subs s 0 (min n (count s)))))

(defn -main [& argv]
  (let [{:keys [sel agents?]} (r/args argv)]
    (doseq [s (r/sessions sel :agents? agents?)
            :let [evals (keep (fn [c]
                                (let [k (str (get-in c [:input :command]))]
                                  (cond (= "mcp:clojure_eval" (r/short-tool (:tool c))) [c (get-in c [:input :code])]
                                        (re-find #"cljdev eval" k) [c (str/replace k #"^.*?cljdev eval (--dir \S+ )?" "")])))
                              (:calls s))]
            :when (seq evals)]
      (println "\n==" (r/project-name (:dir s)) (r/short-id (:id s)) (:start s) "·" (count evals) "evals")
      (doseq [[c code] evals]
        (println (format "  %3d %s %s" (:n c) (if (:error? c) "✗" " ") (head code 150)))))))
