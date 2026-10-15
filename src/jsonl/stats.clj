(ns jsonl.stats
  "Tables over transcripts (selection as in jsonl.read/files).

  bb jsonl:ls    [sel]   one line per transcript: start, project, size, first prompt
  bb jsonl:stats [sel]   per session: prompts, tool calls, failures, tokens; then totals
  bb jsonl:calls [sel]   every tool call as one edn map per line, for your own digging"
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jsonl.read :as r]))

(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))
(defn- day [ts] (if ts (str/replace (subs ts 0 (min 16 (count ts))) "T" " ") "?"))
(defn- k [n] (cond (nil? n) "-" (>= n 1000000) (format "%.1fM" (/ n 1e6)) (>= n 1000) (format "%.0fk" (/ n 1e3)) :else (str n)))

(defn- first-prompt
  "The first prompt of a transcript, reading only as far as needed."
  [f]
  (with-open [rd (io/reader f)]
    (some #(r/prompt-text (try (json/parse-string % true) (catch Exception _ nil))) (line-seq rd))))

(defn ls [& argv]
  (let [{:keys [sel agents?]} (r/args argv)]
    (doseq [f (sort-by #(fs/last-modified-time %) (r/files sel :agents? agents?))]
      (let [dir (r/dir-of f nil)]
        (println (format "%s  %-28s %6s  %s  %s"
                         (day (str (.toInstant (fs/last-modified-time f)))) (clip (r/project-name dir) 28)
                         (k (fs/size f)) (subs (str (fs/file-name f)) 0 8) (clip (first-prompt f) 70)))))))

(defn- row [s]
  (let [{:keys [input_tokens output_tokens cache_read_input_tokens cache_creation_input_tokens]} (:usage s)
        calls (:calls s)]
    {:start (day (:start s)) :project (r/project-name (:dir s)) :id (r/short-id (:id s))
     :prompts (count (:prompts s)) :calls (count calls) :failed (count (filter :error? calls))
     :out output_tokens :in input_tokens :cache-read cache_read_input_tokens :cache-write cache_creation_input_tokens
     :result-chars (reduce + (keep :result-chars calls))
     :models (str/join "," (distinct (keep #(some-> (:model %) (str/replace #"^claude-" "")) (:messages s))))}))

(def ^:private cols
  [[:start "start" 16] [:project "project" 24] [:id "session" 8] [:prompts "prompts" 7] [:calls "calls" 6]
   [:failed "failed" 6] [:out "out-tok" 8] [:cache-read "cache-rd" 9] [:cache-write "cache-wr" 9]
   [:result-chars "out-chars" 9] [:models "models" 0]])

(defn- print-row [m]
  (println (str/join "  " (for [[key _ w] cols :let [v (get m key)
                                                     v (if (and (number? v) (#{:out :in :cache-read :cache-write :result-chars} key)) (k v) (str v))]]
                            (if (zero? w) v (format (str "%-" w "s") (clip v w)))))))

(defn stats [& argv]
  (let [{:keys [sel agents?]} (r/args argv)
        rows (sort-by :start (map row (r/sessions sel :agents? agents?)))]
    (print-row (into {} (for [[key h] cols] [key h])))
    (run! print-row rows)
    (when (> (count rows) 1)
      (print-row (merge (into {} (for [key [:prompts :calls :failed :out :in :cache-read :cache-write :result-chars]]
                                   [key (reduce + (keep key rows))]))
                        {:start "total" :project (str (count rows) " sessions") :id "" :models ""})))))

(defn calls [& argv]
  (let [{:keys [sel agents?]} (r/args argv)]
    (doseq [s (r/sessions sel :agents? agents?) c (:calls s)]
      (prn (assoc c :session (:id s) :project (r/project-name (:dir s)))))))
