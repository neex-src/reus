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

(defn mains
  "The projects: those with a <project>/main."
  [tips]
  (sort (keep #(second (re-matches #"(.+)/main" %)) (keys tips))))

(defn- branches-of [tips main]
  (filter #(or (str/starts-with? % (str main "/s/")) (str/starts-with? % (str main "/a/"))) (keys tips)))

(defn- suffix [branch] (second (re-matches #".*/[sa]/(.+)" branch)))

(def ^:private notes-cache (atom {}))
(def ^:private RS "\u001e")
(def ^:private US "\u001f")
(def ^:private delim "\n\n--- error ---\n")

(def ^:private subjects-cache (atom {}))

(defn own-commits
  "[{:sha :at :subject}] of a session or agent branch, newest first: its own first-parent
  commits, up to the first one that mirrors the real main (or the root of the trace); an agent
  branch, which is cut from its parent's tip, ends at its own first commit."
  [tip & [agent?]]
  (or (get @subjects-cache [tip agent?])
      (let [recs (remove str/blank? (str/split (git "log" "--first-parent" "-n" "20000" (str "--format=%H" US "%at" US "%s" US "%b" RS) tip) (re-pattern RS)))
            v (vec (take-while #(not (or (re-find #"(?m)^Real-Commit: " (str (nth % 3 ""))) (str/starts-with? (str/triml (nth % 2 "")) "trace of ")))
                               (map #(str/split (str/triml %) (re-pattern US) -1) recs)))
            v (mapv (fn [[sha at s]] {:sha sha :at (parse-long at) :subject s}) v)
            v (if agent?
                (let [[before [p]] (split-with #(not (re-find #"^prompt [\w]+-a\w+:" (:subject %))) v)] (vec (concat before (when p [p]))))
                v)]
        (when (> (count @subjects-cache) 200) (reset! subjects-cache {}))
        (swap! subjects-cache assoc [tip agent?] v)
        v)))

(defn log-records
  "The records of the given commits (shas), with their numstat, in the order given."
  [shas]
  (when (seq shas)
    (->> (str/split (git "log" "--no-walk=unsorted" "--stdin" "--numstat"
                         (str "--format=" RS "%H" US "%P" US "%at" US "%s" US "%b" US)
                         {:in (str (str/join "\n" shas) "\n")})
                    (re-pattern RS))
         (remove str/blank?)
         (map (fn [rec]
                (let [[sha ps at s b stat] (str/split rec (re-pattern US) -1)]
                  {:sha sha :parents (str/split ps #" ") :at (parse-long at) :subject s
                   :body (str/trim-newline (or b ""))
                   :files (vec (keep #(let [[a d path] (str/split % #"\t" 3)]
                                        (when path {:path path :add (parse-long a) :del (parse-long d)}))
                                     (str/split-lines (or stat ""))))}))))))

(defn prompt-id [branch nn] (str (suffix branch) "@" nn))

(defn prompt-nos
  "The prompts of a project, as ids <session8>@NN (a session) or <session8>-<agent>@NN-a<agent>
  (an agent run): every prompt of every session and agent branch, oldest first."
  [tips main]
  (->> (branches-of tips main)
       (mapcat (fn [b] (map (fn [r] [(:at r) (prompt-id b (second (re-find #"^prompt ([\w-]+):" (:subject r))))])
                            (filter #(re-find #"^prompt [\w-]+:" (:subject %)) (own-commits (get tips b) (str/includes? b "/a/"))))))
       (sort-by first)
       (mapv second)))

(defn- branch-of-id [main id]
  (let [[suf _] (str/split id #"@" 2)]
    (str main (if (str/includes? suf "-") "/a/" "/s/") suf)))

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


(def ^:private read-tools #{"Read" "read_file" "NotebookRead"})
(def ^:private edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})
(def ^:private trace-dir ".claude-trace/")

(defn split-trailers
  "[text trailers-map] of a commit body: its last paragraph when every line of it is `Key: value`."
  [body]
  (let [paras (str/split (str/trim-newline (str body)) #"\n\n")
        lastp (last paras)
        lines (str/split-lines (str lastp))]
    (if (and (seq paras) (seq lines) (every? #(re-find #"^[A-Z][A-Za-z-]*: " %) lines))
      [(str/join "\n\n" (butlast paras))
       (reduce (fn [m l] (let [[_ k v] (re-matches #"([A-Za-z-]+): (.*)" l)] (update m k (fn [o] (if o (if (vector? o) (conj o v) [o v]) v)))))
               {} lines)]
      [(str/trim-newline (str body)) {}])))

(defn classify [r]
  (let [s (:subject r)]
    (cond
      (re-find #"^prompt [\w-]+:" s) :prompt
      (re-matches #"response [\w-]+" s) :response
      :else :tool)))

(defn- real-files [r] (vec (remove #(str/starts-with? (:path %) trace-dir) (:files r))))

(defn step [rel r]
  (let [base (classify r)
        [cmd tr] (split-trailers (:body r))
        tool (get tr "Tool")
        call (when (= base :tool) (parse-call (rel cmd)))
        real (real-files r)
        file (get tr "File")
        kind (cond (not= base :tool) base
                   (seq real) :edit
                   (read-tools tool) :read
                   (edit-tools tool) :edit
                   file :read
                   :else :run)]
    (merge (select-keys r [:sha :at :subject])
           {:kind kind
            :cmd (rel cmd)
            :agent (get tr "Agent")
            :call call
            :err (get tr "Error")
            :trailers tr
            :read-file (when (= kind :read) file)
            :blob (when (and (= kind :read) file) (str (:sha r) ":" file))
            :files real
            :add (reduce + 0 (keep :add real))
            :del (reduce + 0 (keep :del real))})))

(defn relativizer
  "Strip the session's checkout path out of commands, so they read as the agent
  wrote them relative to its cwd: the root a trace.record prompt names."
  [roots]
  (fn [s] (reduce #(-> %1 (str/replace (str "cd " %2 " && ") "") (str/replace (str %2 "/") "") (str/replace %2 "."))
                  (str s) roots)))

(defn note-blobs
  "{commit blob-sha} of the commits among shas that have a note in refs/notes/<name>, from the
  note list, which is kept per tip of the notes ref."
  [name shas]
  (let [ref (str "refs/notes/" name)
        tip (str/trim (:out (g/git* {} "rev-parse" "-q" "--verify" ref)))]
    (when-not (str/blank? tip)
      (let [m (or (get @notes-cache [g/*repo* ref tip])
                  (let [m (into {} (keep #(let [[blob c] (str/split % #" ")] (when c [c blob])))
                                (str/split-lines (git "notes" (str "--ref=" name) "list")))]
                    (swap! notes-cache (fn [c] (assoc (into {} (remove (fn [[k _]] (= ref (second k))) c)) [g/*repo* ref tip] m)))
                    m))]
        (into {} (keep (fn [c] (when-let [b (get m c)] [c b])) shas))))))

(defn turn-notes
  "{commit data} of the commits among shas that have a turn note (what the transcript added: trace.turn)."
  [shas]
  (let [want (note-blobs "turn" shas)
        blobs (cat-batch (vals want))]
    (into {} (keep (fn [[c b]] (when-let [t (get blobs b)] [c (try (edn/read-string t) (catch Exception _ nil))])) want))))

(def ^:private cache (atom {}))

(defn prompt-data*
  [main nn tip branch]
  (let [recs (let [rs (reverse (own-commits tip (str/includes? branch "/a/")))
                   [_ [p & more]] (split-with #(not (str/starts-with? (:subject %) (str "prompt " nn ":"))) rs)]
               (when p (log-records (map :sha (cons p (take-while #(not (re-find #"^prompt [\w-]+:" (:subject %))) more))))))
        head (some->> (first recs) :body (re-seq #"(?m)^(session|cwd|root) (.+)$") (into {} (map (comp vec rest))))
        rel (relativizer (keep #(get head %) ["root"]))
        steps (mapv (partial step rel) recs)
        tn (turn-notes (map :sha steps))
        outs (note-blobs "output" (map :sha steps))
        texts (cat-batch (concat (keep #(when (#{:prompt :response} (:kind %)) (str (:sha %) ":" trace-dir (if (= :prompt (:kind %)) "prompt.md" "reply.md"))) steps)
                                 (vals outs)))
        steps (mapv (fn [s]
                      (let [o (get outs (:sha s)) t (when o (get texts o))]
                        (cond-> s
                          (get tn (:sha s)) (assoc :turn (get tn (:sha s)))
                          o (assoc :result o)
                          t (assoc :lines (count (str/split-lines t)))
                          (and t (= "Agent" (:tool (:call s)))) (assoc :agent-id (agents/agent-id t))
                          (and o (nil? t)) (assoc :result nil :same true))))
                    steps)
        prompt (first (filter #(= :prompt (:kind %)) steps))
        response (first (filter #(= :response (:kind %)) steps))
        work (vec (map-indexed #(assoc %2 :i (inc %1)) (remove #(#{:prompt :response} (:kind %)) steps)))]
    {:main main :nn (prompt-id branch nn) :branch branch :tip tip
     :session (get head "session") :cwd (get head "cwd") :root (get head "root")
     :prompt (some-> prompt :sha (str ":" trace-dir "prompt.md") texts)
     :reply (some-> response :sha (str ":" trace-dir "reply.md") texts)
     :turn (:turn prompt) :turn-end (:turn response)
     :trailers (:trailers prompt)
     :t0 (:at prompt) :t1 (or (:at response) (:at (peek work)) (:at prompt))
     :fork (some-> prompt :sha (str "^"))
     :steps work}))

(defn prompt-data
  "One prompt of a session or agent branch of a project, by the id prompt-nos gives."
  [tips main id]
  (let [[_ nn] (str/split id #"@" 2)
        b (branch-of-id main id) tip (get tips b)]
    (when (and tip nn)
      (or (get @cache [b nn tip])
          (let [d (prompt-data* main nn tip b)] (swap! cache assoc [b nn tip] d) d)))))

(defn central?
  "A trace.record repo (live: it keeps session state), as opposed to a bench trace.bb repo."
  []
  (fs/exists? (fs/path g/*repo* ".git" "trace-sessions")))

(defn live-branches
  "Ids (see prompt-nos) of the prompts that are still being worked on: open in a session (trace.record)."
  []
  (set (let [d (fs/path g/*repo* ".git" "trace-sessions")]
         (when (fs/exists? d)
           (keep #(try (let [st (edn/read-string (slurp (str %)))]
                         (when (:open? st) (prompt-id (:branch st) (:nn st))))
                       (catch Exception _ nil))
                 (fs/list-dir d))))))

(defn all-prompts [tips main]
  (keep #(prompt-data tips main %) (prompt-nos tips main)))
