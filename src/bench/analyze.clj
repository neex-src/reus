(ns bench.analyze
  "Categorise every tool call of analysis/<s>-calls.edn (from bench:extract) and print the tables."
  (:require [bench.env :as env] [clojure.edn :as edn] [clojure.string :as str]))

(defn calls [s] (map edn/read-string (str/split-lines (slurp (env/calls-file s)))))
(defn short-tool [t] (str/replace t #"^mcp__clj-um-\w+__" "mcp:"))
(defn cmd [c] (or (get-in c [:input :command]) ""))
(defn category [{:keys [tool input] :as c}]
  (let [t (short-tool tool) k (cmd c)]
    (cond
      (#{"Edit" "mcp:file_edit" "mcp:clojure_edit" "mcp:clojure_edit_replace_sexp" "mcp:file_write" "Write"} t)
      (keyword (str "edit/" (name (keyword (str/replace t #"mcp:" "tool-"))) ))
      (#{"Read" "mcp:read_file" "mcp:grep" "mcp:glob_files" "mcp:deps_list" "mcp:clojure_inspect_project" "Grep" "Glob"} t) :explore/tool
      (= t "mcp:clojure_eval") (if (re-find #"c/stop|component/stop|\(stop " k) :eval/restart-system
                                   (if (re-find #"reload|require .*:reload" (str (:code input))) :reload :eval/call))
      (or (= t "Bash") (= t "mcp:bash"))
      (cond
        (re-find #"python3? " k) :edit/python-heredoc
        (re-find #"sed -i|perl -pi" k) :edit/sed
        (re-find #"cat\s*>>?\s*\S+\s*<<|tee |>>\s*\S+" k) :edit/heredoc-write
        (re-find #"cljdev test" k) :test
        (re-find #"cljdev reload" k) :reload
        (and (re-find #"cljdev eval" k) (re-find #"c/stop|component/stop" k)) :eval/restart-system
        (re-find #"cljdev eval" k) :eval/call
        (re-find #"git (commit|add|checkout|reset)" k) :git/write
        (re-find #"git " k) :git/read
        (re-find #"curl" k) :http-check
        (re-find #"\b(grep|rg|cat|head|tail|sed -n|ls|find|wc|awk)\b" k) :explore/shell
        :else :other/shell)
      :else :other/other)))
(defn segs [k] (inc (count (re-seq #";|&&|\|\|" k))))

(defn -main [& args]
  (doseq [s env/sessions]
    (let [cs (map #(assoc % :cat (category %)) (calls s))
          by (group-by :cat cs)]
      (println "\n===" s (count cs) "calls")
      (doseq [[c v] (sort-by (comp - count val) by)]
        (printf "  %-28s %3d   errors %d\n" c (count v) (count (filter :error v))))
      (let [shell (filter #(#{"Bash" "mcp:bash"} (short-tool (:tool %))) cs)]
        (printf "  shell cmds: %d, avg pipeline segments %.1f, avg chars %.0f\n" (count shell)
                (/ (reduce + (map #(segs (cmd %)) shell)) (double (max 1 (count shell))))
                (/ (reduce + (map #(count (cmd %)) shell)) (double (max 1 (count shell))))))
      (println "  calls per round (rounds 1..32 with any edit/*):")
      (println "   " (->> cs (group-by :round) (sort-by key)
                           (map (fn [[r v]] (str r ":" (count v) (if (some #(= "edit" (namespace (:cat %))) v) "e" ""))))
                           (str/join " "))))))
