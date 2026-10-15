(ns jsonl.analyze
  "Every tool call put in a category (what it was for and how it was done), then tabled.

  bb jsonl:analyze [sel] [--each] [--agents]
  One table over all selected sessions (--each: one per session): calls, failures, time
  and output size per category, shell pipeline length, and calls per prompt."
  (:require [clojure.string :as str]
            [jsonl.read :as r]))

(defn- cmd [c] (str (or (get-in c [:input :command]) "")))

(defn category [{:keys [tool input] :as c}]
  (let [t (r/short-tool tool) k (cmd c)]
    (cond
      (#{"Edit" "MultiEdit" "Write" "NotebookEdit" "mcp:file_edit" "mcp:clojure_edit" "mcp:clojure_edit_replace_sexp" "mcp:file_write"} t)
      (keyword "edit" (str/replace t #"^mcp:" "tool-"))
      (#{"Read" "Grep" "Glob" "mcp:read_file" "mcp:grep" "mcp:glob_files" "mcp:deps_list" "mcp:clojure_inspect_project"} t) :explore/tool
      (= t "mcp:clojure_eval") (if (re-find #"c/stop|component/stop|\(stop " (str (:code input))) :eval/restart-system
                                   (if (re-find #"reload|require .*:reload" (str (:code input))) :eval/reload :eval/call))
      (#{"Bash" "mcp:bash"} t)
      (cond
        (re-find #"python3? " k) :edit/python-heredoc
        (re-find #"sed -i|perl -pi" k) :edit/sed
        (re-find #"cat\s*>>?\s*\S+\s*<<|tee |>>\s*\S+" k) :edit/heredoc-write
        (re-find #"cljdev test" k) :test
        (re-find #"cljdev reload" k) :eval/reload
        (and (re-find #"cljdev eval" k) (re-find #"c/stop|component/stop" k)) :eval/restart-system
        (re-find #"cljdev eval" k) :eval/call
        (re-find #"git (commit|add|checkout|reset|merge|rebase|worktree)" k) :git/write
        (re-find #"git " k) :git/read
        (re-find #"curl" k) :http-check
        (re-find #"\b(grep|rg|cat|head|tail|sed -n|ls|find|wc|awk)\b" k) :explore/shell
        :else :other/shell)
      (#{"Agent" "Task"} t) :agent
      (#{"WebSearch" "WebFetch"} t) :web
      :else (keyword "other" t))))

(defn- segs [k] (inc (count (re-seq #";|&&|\|\|" k))))
(defn- avg [xs] (if (seq xs) (/ (reduce + xs) (double (count xs))) 0.0))

(defn table [title calls prompts]
  (let [cs (map #(assoc % :cat (category %)) calls)]
    (println "\n===" title "·" (count cs) "calls ·" prompts "prompts")
    (println (format "  %-26s %6s %7s %9s %10s" "category" "calls" "failed" "avg s" "avg chars"))
    (doseq [[c v] (sort-by (comp - count val) (group-by :cat cs))]
      (println (format "  %-26s %6d %7d %9.1f %10.0f" (str (namespace c) (when (namespace c) "/") (name c)) (count v)
                       (count (filter :error? v)) (/ (avg (keep :ms v)) 1000) (avg (keep :result-chars v)))))
    (let [shell (filter #(#{"Bash" "mcp:bash"} (r/short-tool (:tool %))) cs)]
      (println (format "  shell: %d calls, avg %.1f commands each, avg %.0f chars" (count shell)
                       (avg (map #(segs (cmd %)) shell)) (avg (map #(count (cmd %)) shell)))))
    cs))

(defn -main [& argv]
  (let [{:keys [sel agents? each?]} (r/args argv)
        ss (r/sessions sel :agents? agents?)]
    (if each?
      (doseq [s ss]
        (let [cs (table (str (r/project-name (:dir s)) " " (r/short-id (:id s)) " " (:start s))
                        (:calls s) (count (:prompts s)))]
          (println "  calls per prompt (e = it edited):")
          (println "   " (->> cs (group-by :n) (sort-by key)
                               (map (fn [[n v]] (str n ":" (count v) (when (some #(= "edit" (namespace (:cat %))) v) "e"))))
                               (str/join " ")))))
      (table (str (count ss) " sessions") (mapcat :calls ss) (reduce + (map (comp count :prompts) ss))))))
