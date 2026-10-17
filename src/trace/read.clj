(ns trace.read
  "A trace repo as data: its mains (a branch B with a B-p00 beside it), their
  prompts, and each prompt's steps, one per tool call, classified from how
  trace.bb / trace.record encode them. Per-branch results are cached by tip."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.agents :as agents]
            [trace.git :as g :refer [git cat-batch]]))

(defn tips
  "{branch sha} for every local branch."
  []
  (into (sorted-map)
        (for [l (str/split-lines (git "for-each-ref" "--format=%(refname:short) %(objectname)" "refs/heads"))
              :let [[b s] (str/split l #" ")] :when s]
          [b s])))

(defn worktrees
  "{branch path} of every worktree."
  []
  (->> (str/split (git "worktree" "list" "--porcelain") #"\n\n")
       (keep #(let [path (second (re-find #"(?m)^worktree (.+)$" %))
                    b (second (re-find #"(?m)^branch refs/heads/(.+)$" %))]
                (when (and path b) [b path])))
       (into {})))

(defn mains [tips]
  (sort (filter #(contains? tips (str % "-p00")) (keys tips))))

(defn prompt-nos [tips main]
  (sort-by parse-long (keep #(second (re-matches (re-pattern (str "\\Q" main "\\E-p(\\d+)")) %)) (keys tips))))

;; --- one prompt branch -> steps ---------------------------------------------

(def ^:private RS "\u001e")
(def ^:private US "\u001f")
(def ^:private delim "\n\n--- error ---\n")

(defn log-records [rev]
  (->> (str/split (git "log" "--first-parent" "-n" "5000" "--numstat"
                       (str "--format=" RS "%H" US "%P" US "%at" US "%s" US "%b" US) rev)
                  (re-pattern RS))
       (remove str/blank?)
       (map (fn [rec]
              (let [[sha ps at s b stat] (str/split rec (re-pattern US) -1)]
                {:sha sha :parents (str/split ps #" ") :at (parse-long at) :subject s
                 :body (str/trim-newline (or b ""))
                 :files (vec (keep #(let [[a d path] (str/split % #"\t" 3)]
                                      (when path {:path path :add (parse-long a) :del (parse-long d)}))
                                   (str/split-lines (or stat ""))))})))))

(defn prompt-commit? [nn r]
  (and (str/starts-with? (:subject r) (str "prompt " nn ":")) (= 1 (count (:parents r)))))

(defn ext-lang [path]
  (let [e (some-> (re-find #"\.([A-Za-z0-9]+)$" (str path)) second str/lower-case)]
    (get {"clj" "clojure" "cljs" "clojure" "cljc" "clojure" "edn" "clojure" "bb" "clojure"
          "html" "xml" "xml" "xml" "svg" "xml" "css" "css" "md" "markdown" "json" "json"
          "py" "python" "sh" "bash" "js" "javascript" "sql" "sql" "yml" "yaml" "yaml" "yaml"}
         e "plaintext")))

(defn parse-call
  "What was executed: {:tool name :input map} for a tool call recorded as
  `<tool> {json}`, else {:tool bash|eval :code text :lang ..}."
  [cmd]
  (or (when-let [[_ tool js] (re-matches #"(?s)((?:mcp__\S+__)?[A-Za-z_]+) (\{.*\})" cmd)]
        (when-let [input (try (json/parse-string js) (catch Exception _ nil))]
          {:tool (str/replace tool #"^mcp__.*__" "") :mcp? (str/starts-with? tool "mcp__") :input input}))
      (if (re-find #"^\s*(\(|;;|\[|\{|#\{|#_|:[a-z])" cmd)
        {:tool "eval" :code cmd :lang "clojure"}
        {:tool "bash" :code cmd :lang "bash"})))

(defn classify [r]
  (let [s (:subject r)
        real (remove #(or (str/starts-with? (:path %) "reads/") (= "doto.md" (:path %))) (:files r))]
    (cond
      (re-find #"^prompt \d+:" s) :prompt
      (re-matches #"response \d+" s) :response
      (str/starts-with? s "baseline ") :baseline
      (seq real) :edit
      (re-matches #"[0-9a-f]{40}" s) :read
      :else :run)))

(def ^:private read-tools #{"Read" "read_file" "NotebookRead"})
(def ^:private edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})

(defn step [rel r]
  (let [base (classify r)
        [cmd err] (str/split (:body r) (re-pattern (java.util.regex.Pattern/quote delim)) 2)
        call (when (#{:read :run :edit} base) (parse-call (rel cmd)))
        ;; a call on a file outside the traced tree changes nothing there: it is
        ;; recorded as a run, but its tool says what it was
        kind (cond (not= base :run) base
                   (read-tools (:tool call)) :read
                   (edit-tools (:tool call)) :edit
                   :else :run)
        reads-path (some #(when (str/starts-with? (:path %) "reads/") (:path %)) (:files r))
        result (cond
                 (= base :read) reads-path
                 (= base :run) (or reads-path (str "reads/_cmd/" (:subject r))))
        real (vec (remove #(or (str/starts-with? (:path %) "reads/") (= "doto.md" (:path %))) (:files r)))]
    (merge (select-keys r [:sha :at :subject])
           {:kind kind
            :cmd (rel cmd)
            :call call
            :err err
            :result result
            :read-file (when (= base :read) (some-> reads-path (subs 6)))
            :blob (when (= base :read) (:subject r))
            :files real
            :add (reduce + 0 (keep :add real))
            :del (reduce + 0 (keep :del real))})))

(defn relativizer
  "Strip the session's checkout path out of commands, so they read as the agent
  wrote them relative to its cwd: the worktree that has main checked out (trace.bb),
  and the root a trace.record prompt names."
  [main roots]
  (let [paths (concat roots (keep (fn [[b p]] (when (or (= b main) (str/starts-with? b (str main "-p"))) p)) (worktrees)))]
    (fn [s] (reduce #(-> %1 (str/replace (str "cd " %2 " && ") "") (str/replace (str %2 "/") "") (str/replace %2 "."))
                    (str s) paths))))

(def ^:private cache (atom {}))

(defn prompt-data*
  [main nn tip]
  (let [recs (reverse (let [rs (log-records tip)
                            [before [p]] (split-with #(not (prompt-commit? nn %)) rs)]
                        (if p (concat before [p]) before)))
        head (some->> (first recs) :body (re-seq #"(?m)^(session|cwd|root) (.+)$") (into {} (map (comp vec rest))))
        rel (relativizer main (keep #(get head %) ["root"]))
        steps (mapv (partial step rel) recs)
        texts (cat-batch (concat (keep #(when (#{:prompt :response} (:kind %)) (str (:sha %) ":doto.md")) steps)
                                 (keep #(when (:result %) (str (:sha %) ":" (:result %))) steps)))
        steps (mapv (fn [s]
                      (let [t (when (:result s) (get texts (str (:sha s) ":" (:result s))))]
                        (cond-> s
                          t (assoc :lines (count (str/split-lines t)))
                          (and t (= "Agent" (:tool (:call s)))) (assoc :agent-id (agents/agent-id t))
                          (and t (str/starts-with? t "--- error ---")) (assoc :err true)
                          (and (:result s) (nil? t)) (assoc :result nil :same true))))
                    steps)
        prompt (first (filter #(= :prompt (:kind %)) steps))
        response (first (filter #(= :response (:kind %)) steps))
        work (vec (map-indexed #(assoc %2 :i (inc %1)) (remove #(#{:prompt :response :baseline} (:kind %)) steps)))]
    {:main main :nn nn :tip tip
     :session (get head "session") :cwd (get head "cwd") :root (get head "root")
     :prompt (some-> prompt :sha (str ":doto.md") texts)
     :reply (some-> response :sha (str ":doto.md") texts)
     :t0 (:at prompt) :t1 (or (:at response) (:at (peek work)) (:at prompt))
     :fork (some-> prompt :sha (str "^"))
     :steps work}))

(defn prompt-data [tips main nn]
  (let [b (str main "-p" nn) tip (get tips b)]
    (when tip
      (or (get @cache [b tip])
          (let [d (prompt-data* main nn tip)] (swap! cache assoc [b tip] d) d)))))

(defn central?
  "A trace.record repo (one branch per project), as opposed to a bench trace.bb repo."
  []
  (fs/exists? (fs/path g/*repo* ".git" "trace-sessions")))

(defn live-branches
  "Prompt branches whose turn is still running: checked out in a worktree (trace.bb),
  or open in a session (trace.record)."
  []
  (into (set (filter #(re-find #"-p\d+$" %) (keys (worktrees))))
        (let [d (fs/path g/*repo* ".git" "trace-sessions")]
          (when (fs/exists? d)
            (keep #(try (:branch (edn/read-string (slurp (str %)))) (catch Exception _ nil)) (fs/list-dir d))))))

(defn all-prompts [tips main]
  (keep #(prompt-data tips main %) (prompt-nos tips main)))

