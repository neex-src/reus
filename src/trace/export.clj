(ns trace.export
  "One session of a trace repo as static files that open from a folder:

    bb trace:export SESSION DIR [--repo DIR] [--redact FILE] [--cap-diff N] [--cap-note N]

  SESSION is the first 8 characters of a session id. DIR gets
    index.html     the session: its numbers, its prompts, its subagent runs, what was left out
    p/NN.html      one page per prompt: what went in, every step (tool, trailers, the diff of a step that changed files,
                   its output), the answer, tokens and cost. This is the page of one model call
    a/<agent>.html the same for a subagent run
    graph.json     every commit of the session and its agents as data (message, trailers, files, diff, notes, size of its
                   tree, parents, the hand-back links), for a picture of the commit graph made elsewhere
  No server, no script, no font or library from the net (trace.page), nothing scrolls inside a page.

  What is read: ONLY the branches of this session (`<project>/s/<id>` and `<project>/a/<id>-*`, own commits only, never the
  history the session was cut from) and the notes of those commits, one `git log --no-walk` per note ref. No other ref is listed.

  What is never written: everything under `.claude-trace/` (every snapshot carries the owner's instruction files and the project's
  memory files there). prompt.md and reply.md are read for the prompt and the answer; for the rest only the number of files in
  each tree is kept.

  --redact FILE: an edn vector of {:re \"regex\" :to \"replacement\" :why \"what it is\"}, applied to every text before it is
  written. Every cut is counted and listed on index.html, so nothing is cut silently.

  A subagent's report does not come back as a git merge: the next prompt of the session carries it (`<agent-message from=...>`)
  and the end-of-turn commit has an `Agent-Report: <tip of the agent>` trailer, which is no parent link. graph.json says so in :links."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [trace.git :as g :refer [git]]
            [trace.page :as pg :refer [thou clip]]
            [trace.read :as r]
            [trace.runs :as runs]))

(def ^:dynamic *cap-diff* "Characters of a diff kept in graph.json." 3000)
(def ^:dynamic *cap-note* "Characters of a note kept in graph.json." 1500)
(def page-cap "Characters of one diff or output on a page; the rest is said, not shown." 60000)

(defn private?
  "A path of the trace's own snapshot (.claude-trace/): the prompt, the answer, the instruction files, the memory."
  [path] (str/starts-with? (str path) ".claude-trace/"))

(def read-tools #{"Read" "Grep" "Glob" "NotebookRead" "read_file"})

(defn kind-of
  "The kind of a commit: :prompt (typed), :brief (the first commit of a subagent's branch: the brief its session gave it), :notice-agent (an agent's hand-back that Claude Code puts in as a prompt),
  :notice-task (a background task's notification, the same), :end, :agent (a subagent starts), :handback (a subagent
  ends), :write (the tree changed), :read, :command."
  [{:keys [base tool real? text agent?]}]
  (case base
    :prompt (let [t (str/triml (str text))]
              (cond agent? :brief
                    (str/starts-with? t "<agent-message ") :notice-agent
                    (str/starts-with? t "<task-notification>") :notice-task
                    :else :prompt))
    :response :end
    (cond (= "Agent" tool) :agent
          (= "SubagentHandback" tool) :handback
          real? :write
          (read-tools tool) :read
          :else :command)))

;; --- redaction --------------------------------------------------------------------------

(defn redactor
  "[fn counts-atom]: fn cuts every match of every rule out of a string; counts is {why #{distinct texts cut}}."
  [rules]
  (let [counts (atom {})
        rs (mapv (fn [{:keys [re to why]}] [(re-pattern re) (or to "[cut]") (or why re)]) rules)]
    [(fn [s]
       (if (string? s)
         (reduce (fn [s [re to why]]
                   (let [ms (map #(if (vector? %) (first %) %) (re-seq re s))]
                     (if (seq ms) (do (swap! counts update why (fnil into #{}) ms) (str/replace s re to)) s)))
                 s rs)
         s))
     counts]))

(defn- walk-strings [f x]
  (cond (string? x) (f x)
        (map? x) (into (empty x) (map (fn [[k v]] [k (walk-strings f v)])) x)
        (vector? x) (mapv #(walk-strings f %) x)
        (sequential? x) (doall (map #(walk-strings f %) x))
        :else x))

;; --- reading the session ----------------------------------------------------------------

(defn session-branches
  "The branches of session8: [{:branch :tip :project :agent? :agent}], only those, found by pattern."
  [sid8]
  (let [out (git "for-each-ref" "--format=%(refname:short) %(objectname)" (str "refs/heads/*/s/" sid8) (str "refs/heads/*/a/" sid8 "-*"))
        re (re-pattern (str "(.+)/(s|a)/(" sid8 "(?:-.+)?)"))]
    (vec (for [l (remove str/blank? (str/split-lines out))
               :let [[b sha] (str/split l #" ")
                     [_ project kind suffix] (re-matches re b)]
               :when project]
           {:branch b :tip sha :project project :agent? (= "a" kind)
            :agent (when (= "a" kind) (subs suffix (inc (count sid8))))}))))

(defn trailer-list
  "[[key value]] in the order written: the last paragraph of a message when every line of it is `Key: value`."
  [body]
  (let [paras (str/split (str/trim-newline (str body)) #"\n\n")
        lines (str/split-lines (str (last paras)))]
    (if (and (seq paras) (seq lines) (every? #(re-find #"^[A-Z][A-Za-z-]*: " %) lines))
      (mapv #(vec (rest (re-matches #"([A-Za-z-]+): (.*)" %))) lines)
      [])))

(defn- notes-of
  "{sha text} of the notes in refs/notes/<name> of the given commits (one process)."
  [name shas]
  (when (seq shas)
    (into {}
          (keep (fn [rec] (let [[sha t] (str/split rec #"\u001f" 2)]
                            (when-not (str/blank? t) [(str/trim sha) (str/trim-newline t)]))))
          (rest (str/split (git "log" "--no-walk=unsorted" "--stdin" (str "--notes=" name) "--format=%x1e%H%x1f%N"
                                {:in (str (str/join "\n" shas) "\n")})
                           #"\u001e")))))

(defn strip-trailers
  "A message body without its trailer block."
  [body]
  (if (seq (trailer-list body))
    (str/join "\n\n" (butlast (str/split (str/trim-newline (str body)) #"\n\n")))
    (str body)))

(defn- group-of [subject]
  (or (second (re-find #"^prompt ([\w-]+):" subject)) (second (re-find #"^response ([\w-]+)$" subject))
      (second (re-find #"^([\w-]+)\.\d+ " subject))))

(defn- diff-of [sha]
  (git "diff-tree" "-p" "-r" "--no-commit-id" "--no-color" sha "--" "." ":!.claude-trace"))

(defn- tree-files [sha]
  (count (remove private? (remove str/blank? (str/split-lines (git "ls-tree" "-r" "--name-only" sha))))))

(defn- edn-note [t] (try (edn/read-string t) (catch Exception _ nil)))

(defn- usage-of [trailers turn]
  (or (:usage turn)
      (let [t (into {} trailers) n #(some-> (get t %) parse-long)]
        (when (get t "Tokens-Out")
          {:input_tokens (n "Tokens-In") :output_tokens (n "Tokens-Out") :cache_read_input_tokens (n "Cache-Read") :cache_creation_input_tokens (n "Cache-Write")}))))

(defn collect
  "Everything about session8: {:session :project :branches [...] :commits [...] :links [...]}; commits oldest first per branch."
  [sid8]
  (let [bs (session-branches sid8)
        _ (when (empty? bs) (throw (ex-info (str "no branch of session " sid8 " in " g/*repo*) {:session sid8})))
        owns (into {} (map (fn [b] [(:branch b) (vec (reverse (r/own-commits (:tip b) (:agent? b))))])) bs)
        recs (r/log-records (mapcat #(map :sha (owns (:branch %))) bs))
        by-sha (into {} (map (juxt :sha identity)) recs)
        shas (map :sha recs)
        known (set shas)
        texts (g/cat-batch (for [rc recs
                                 :let [base (r/classify rc)] :when (#{:prompt :response} base)]
                             (str (:sha rc) ":.claude-trace/" (if (= :prompt base) "prompt.md" "reply.md"))))
        ev (notes-of "event" shas) tn (notes-of "turn" shas) out (notes-of "output" shas) pr (notes-of "probe" shas)
        commits (vec (for [b bs, c (owns (:branch b))
                           :let [rc (by-sha (:sha c))
                                 base (r/classify rc)
                                 trailers (trailer-list (:body rc))
                                 tmap (into {} trailers)
                                 files (vec (remove #(private? (:path %)) (:files rc)))
                                 text (get texts (str (:sha rc) ":.claude-trace/" (if (= :prompt base) "prompt.md" "reply.md")))
                                 turn (some-> (tn (:sha rc)) edn-note)
                                 kind (kind-of {:base base :tool (get tmap "Tool") :real? (seq files) :text text :agent? (:agent? b)})]]
                       {:sha (:sha rc) :short (subs (:sha rc) 0 10) :branch (:branch b) :agent (:agent b)
                        :group (if (:agent? b) (str "a:" (:agent b)) (group-of (:subject rc)))
                        :base base :kind kind :at (:at rc) :subject (:subject rc)
                        :parents (vec (filter known (:parents rc)))
                        :body (:body rc) :trailers trailers :tool (get tmap "Tool")
                        :files files :add (reduce + 0 (keep :add files)) :del (reduce + 0 (keep :del files))
                        :text text
                        :turn turn :usage (usage-of trailers turn) :model (get tmap "Model")
                        :notes (cond-> {} (ev (:sha rc)) (assoc :event (ev (:sha rc))) (tn (:sha rc)) (assoc :turn (tn (:sha rc)))
                                       (out (:sha rc)) (assoc :output (out (:sha rc))) (pr (:sha rc)) (assoc :probe (pr (:sha rc))))}))
        commits (mapv (fn [c] (cond-> (assoc c :tree-files (tree-files (:sha c)))
                                (seq (:files c)) (assoc :diff (diff-of (:sha c)))))
                      commits)
        links (vec (for [b bs :when (:agent? b)
                         :let [tip (:tip b)
                               trailer-on (some #(when (= tip (get (into {} (:trailers %)) "Agent-Report")) (:sha %)) commits)
                               report-in (some #(when (and (= :notice-agent (:kind %)) (str/includes? (str (:text %)) (str "from=\"" (:agent b) "\""))) (:sha %)) commits)
                               first-c (first (owns (:branch b)))]]
                     {:type "report" :agent (:agent b) :from tip :fork-from (first (:parents (by-sha (:sha first-c))))
                      :to report-in :trailer-on trailer-on
                      :note "a hand-back: the next prompt carries the agent's report and the end-of-turn commit names the agent's tip in an Agent-Report trailer; it is not a git parent"}))]
    {:session sid8 :project (:project (first bs)) :branches (mapv #(select-keys % [:branch :tip :agent? :agent]) bs)
     :commits commits :links links}))

;; --- numbers ---------------------------------------------------------------------------

(defn- sum-usage [us] (apply merge-with + {} (map #(select-keys % [:input_tokens :output_tokens :cache_read_input_tokens :cache_creation_input_tokens :cache_1h_input_tokens]) (remove nil? us))))

(defn- model-of [cs] (some :model cs))

(defn- agent-usage
  "The tokens of an agent run's tool calls, a message counted once (its end-of-turn commit carries no numbers): its last
  answer is not in it."
  [work]
  (let [ms (->> work (filter :usage) (group-by #(or (get-in % [:turn :message-id]) (:sha %))) vals (map (comp :usage first)))]
    (when (seq ms) (sum-usage ms))))

(defn runs-of
  "The prompts of the session and the agent runs, as maps with their commits:
  {:id :agent :n :kind :title :text :reply :commits :work :tools :model :usage :cost :same-turn-as}.
  A session turn's numbers are the end-of-turn commit's (the whole turn). Two end-of-turn commits with the same prompt-id and
  the same numbers are one turn of the transcript: the second says so instead of counting it again."
  [{:keys [commits]}]
  (let [fallback (model-of (filter :tool commits))
        runs (->> (group-by :group commits) vals
                  (map (fn [cs]
                         (let [p (first (filter #(= :prompt (:base %)) cs))
                               e (first (filter #(= :end (:kind %)) cs))
                               work (vec (remove #(#{:prompt :response} (:base %)) cs))
                               agent (:agent (first cs))
                               model (or (model-of work) fallback)
                               usage (if agent (agent-usage work) (:usage e))]
                           {:id (if agent (str "a-" agent) (:group (first cs)))
                            :agent agent :n (when-not agent (:group (first cs)))
                            :kind (:kind p) :text (:text p) :title (clip (first (str/split-lines (str (:text p)))) 160)
                            :reply (:text e) :commits cs :work work
                            :tools (frequencies (keep :tool work))
                            :model model :usage usage
                            :usage-partial? (boolean agent)
                            :turn-key (when-let [pid (:prompt-id (:turn e))] [pid usage])
                            :cost (when usage (runs/cost {:model model :usage usage}))
                            :t0 (:at (first cs)) :t1 (:at (last cs))})))
                  (sort-by (juxt (comp boolean :agent) :n :id)))]
    (loop [[r & more] runs seen {} acc []]
      (if-not r
        acc
        (if-let [first-id (and (:turn-key r) (get seen (:turn-key r)))]
          (recur more seen (conj acc (assoc r :usage nil :cost nil :same-turn-as first-id)))
          (recur more (cond-> seen (:turn-key r) (assoc (:turn-key r) (:id r))) (conj acc r)))))))

(defn numbers [data runs]
  (let [s (remove :agent runs) a (filter :agent runs)
        all-commits (:commits data)
        typed (count (filter #(= :prompt (:kind %)) s))]
    {:prompts (count s) :typed typed :notices (- (count s) typed)
     :agents (count a)
     :commits-session (count (mapcat :commits s)) :commits-agents (count (mapcat :commits a))
     :tools (apply merge-with + (map :tools s)) :agent-tools (apply merge-with + {} (map :tools a))
     :usage (sum-usage (map :usage s))
     :agent-usage (sum-usage (map :usage a))
     :cost (let [cs (keep :cost s)] (when (seq cs) (reduce + cs)))
     :agent-cost (let [cs (keep :cost a)] (when (seq cs) (reduce + cs)))
     :t0 (apply min (map :at all-commits)) :t1 (apply max (map :at all-commits))}))

;; --- the pages -------------------------------------------------------------------------

(def extra-css
  ".diff{padding:0}.diff span{display:block;padding:0 12px}.diff .add{background:#e6f4ea;color:#17803d}.diff .del{background:#fdf3f1;color:#b42318}.diff .hdr{color:#59636e}
.kind{font:600 11px ui-monospace,Menlo,Consolas,monospace;padding:1px 6px;border-radius:4px;background:#eef1f5;color:#2f3a46;white-space:nowrap}
.kind.k-write{background:#e6f4ea;color:#17803d}.kind.k-read{background:#e8f0fe;color:#1a4fb4}.kind.k-agent,.kind.k-handback{background:#f1e8fb;color:#6b3fa0}
.kind.k-notice-agent,.kind.k-notice-task{background:#fdf3e1;color:#8a5a00}.kind.k-prompt{background:#1b1f24;color:#fff}.kind.k-brief{background:#f1e8fb;color:#6b3fa0}
table.tr th{width:9rem;background:none;font:12px ui-monospace,Menlo,Consolas,monospace;color:#59636e;padding:2px 10px 2px 0;border:0}table.tr td{border:0;padding:2px 0;font:12px ui-monospace,Menlo,Consolas,monospace}table.tr{border:0;background:none;width:auto}
.crumb a{color:#3b6fd4}")

(defn- top [prefix]
  [:header.top [:a.brand {:href (str prefix "index.html")} "A session as reus recorded it"]
   [:span {:style "color:#cfd6df"} "static pages: no server, no script"]])

(defn- page [title body prefix] (pg/shell title body :top (top prefix) :extra-css extra-css))

(def kind-name
  {:prompt "prompt" :brief "subagent brief" :notice-agent "hand-back text" :notice-task "task notice" :end "end of turn" :agent "subagent starts"
   :handback "subagent ends" :write "changed files" :read "read" :command "command"})

(defn- kind-tag [k] [:span {:class (str "kind k-" (name k))} (kind-name k)])

(defn- usd [x] (cond (nil? x) "–" (< x 0.01) (format "$%.4f" (double x)) :else (format "$%.2f" (double x))))
(defn- stamp [secs] (when secs (.format (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") (.atZone (java.time.Instant/ofEpochSecond secs) (java.time.ZoneOffset/UTC)))))
(defn- dur [secs] (let [s (max 0 (long secs))] (cond (>= s 3600) (format "%dh %02dm" (quot s 3600) (quot (mod s 3600) 60)) (>= s 60) (format "%d min %02d s" (quot s 60) (mod s 60)) :else (str s " s"))))
(defn- total-in [u] (when u (+ (or (:input_tokens u) 0) (or (:cache_read_input_tokens u) 0) (or (:cache_creation_input_tokens u) 0))))

(defn- cut [s n] (let [s (str s)] (if (> (count s) n) [(subs s 0 n) (- (count s) n)] [s 0])))

(defn- pre-cut
  "A pre with s, at most n characters; what is left over is counted in words."
  [s n & [cls]]
  (let [[t more] (cut s n)]
    (list [:pre (cond-> {} cls (assoc :class cls)) t]
          (when (pos? more) [:p.muted (str "… " (thou more) " more characters not shown here (" (thou (count (str s))) " in all).")]))))

(defn- diff-pre [text n]
  (let [[t more] (cut text n)]
    (list [:pre.diff (for [l (str/split-lines t)]
                       [:span {:class (cond (str/starts-with? l "+++") "hdr" (str/starts-with? l "---") "hdr" (str/starts-with? l "diff ") "hdr" (str/starts-with? l "@@") "hdr"
                                            (str/starts-with? l "+") "add" (str/starts-with? l "-") "del" :else "")} (str l " ")])]
          (when (pos? more) [:p.muted (str "… " (thou more) " more characters of this diff are not shown.")]))))

(defn- step-label [c] (or (second (re-find #"^[\w-]+\.\d+ (.*)$" (:subject c))) (:subject c)))

(defn- step [i c agent-pages]
  (let [out (get-in c [:notes :output]) ev (get-in c [:notes :event])]
    [:details.call {:id (str "c" (:short c))}
     [:summary [:span.i i] [:span.tool (or (:tool c) "")] (kind-tag (:kind c)) [:span.lbl (clip (step-label c) 150)]
      [:span.meta (when-let [o (get (into {} (:trailers c)) "Tokens-Out")] (str (thou (parse-long o)) " tokens out"))
       (when (seq (:files c)) (str " · +" (:add c) " −" (:del c)))]]
     [:div.call-body
      [:div.tag "commit"] [:p [:code (:short c)] " · " (stamp (:at c)) " · the tree of this commit holds " [:b (thou (:tree-files c))] " files (the whole project at that moment)"]
      [:div.tag "message"] (pre-cut (strip-trailers (:body c)) page-cap)
      (when (seq (:trailers c))
        (list [:div.tag "trailers"] [:table.tr (for [[k v] (:trailers c)] [:tr [:th k] [:td v]])]))
      (when-let [ap (get agent-pages (:sha c))] [:p "The subagent's own steps: " [:a {:href (str "../" ap)} "its page"]])
      (when (seq (:files c))
        (list [:div.tag "files changed"] [:ul.plain (for [f (:files c)] [:li [:code (:path f)] (str " +" (:add f) " −" (:del f))])]
              [:div.tag "diff"] (diff-pre (:diff c) page-cap)))
      (when out (list [:div.tag "output (git note \"output\")"] (pre-cut out page-cap)))
      (when-let [t (get-in c [:notes :turn])] (list [:div.tag "turn note (what the transcript added)"] (pre-cut t 6000)))
      (when ev (list [:details [:summary "hook payload (git note \"event\", verbatim)"] (pre-cut ev 8000)]))]]))

(defn- run-page [{:keys [title kind text reply work usage cost model agent n] :as run} data agent-pages nav]
  (let [pages-prefix "../"]
    (page (str (if agent "Subagent run " "Prompt ") (or n agent))
          (list
           [:p.crumb [:a {:href "../index.html"} "session"] (str " › " (if agent (str "subagent " agent) (str "prompt " n)))]
           [:h1 (if agent (str "Subagent run " (subs agent 0 (min 8 (count agent)))) (str "Prompt " n))]
           [:p.muted (kind-tag (if agent :brief (or kind :prompt))) " " (case kind
                                                       :notice-agent "Claude Code put a subagent's report into the session as a prompt; the author did not type it."
                                                       :notice-task "Claude Code put a background task's notification into the session as a prompt; the author did not type it."
                                                       (if agent "The brief the session gave the subagent (the prompt of its Agent call)." "Typed by the author."))]
           [:h2 "1. What went in"]
           [:div.quote text]
           [:h2 (str "2. What it did: " (count work) (if (= 1 (count work)) " step" " steps"))]
           (if (seq work)
             [:div (map-indexed (fn [i c] (step (inc i) c agent-pages)) work)]
             [:p.muted "No tool call: it answered directly."])
           [:h2 "3. What came back"]
           (if reply [:div.quote reply] [:p.muted "No answer recorded."])
           [:h2 "4. What it cost"]
           (cond
             (:same-turn-as run) [:p.muted (str "The transcript has this prompt in the same turn as prompt " (:same-turn-as run) " (the same prompt id and the same numbers): counted there, not again.")]
             usage
             (list [:table
              [:tbody
               [:tr [:td "model"] [:td.n (str/replace (str (or model "–")) #"^claude-" "")]]
               [:tr [:td "tokens in (all of it)"] [:td.n (thou (total-in usage))]]
               [:tr [:td "  of which from cache"] [:td.n (thou (:cache_read_input_tokens usage))]]
               [:tr [:td "tokens out"] [:td.n (thou (:output_tokens usage))]]
               [:tr [:td "cost at list price"] [:td.n (usd cost)]]]]
              (when (:usage-partial? run) [:p.muted "A subagent's end of turn carries no numbers: these are the sum of its tool calls, a message counted once; its last answer is not in it."]))
             :else [:p.muted "No token numbers recorded."])
           (when nav [:p.crumb nav]))
          pages-prefix)))

(defn- index-page [data runs nums cuts]
  (let [ps (remove :agent runs) as (filter :agent runs)
        u (:usage nums)]
    (page (str "Session " (:session data))
          (list
           [:h1 (str "Session " (:session data))]
           [:p.lede (str (:prompts nums)) " prompts (" (:typed nums) " typed by the author, " (:notices nums) " put in by Claude Code), "
            (thou (+ (:commits-session nums) (:commits-agents nums))) " commits, " (:agents nums) " subagent runs, "
            (dur (- (:t1 nums) (:t0 nums))) " from the first commit to the last."]
           [:div.nums
            [:div.num [:b (thou (reduce + 0 (vals (:tools nums))))] [:span "tool calls in the session itself"]]
            [:div.num [:b (thou (reduce + 0 (vals (:agent-tools nums))))] [:span "tool calls inside the subagent runs"]]
            [:div.num [:b (thou (total-in u))] [:span "tokens in, cache included (the session's turns)"]]
            [:div.num [:b (thou (:output_tokens u))] [:span "tokens out"]]
            [:div.num [:b (usd (:cost nums))] [:span "at list price"]]]
           (when (seq as)
             [:p.muted "The subagent runs are separate model calls and are not in those three numbers. Their end-of-turn commit carries no numbers; what the commits of their tool calls carry adds up to "
              (thou (total-in (:agent-usage nums))) " tokens in and " (thou (:output_tokens (:agent-usage nums))) " out (their last answer is not in that), " (usd (:agent-cost nums)) " at list price."])
           [:p.muted "Calls in the session: " (str/join ", " (for [[t n] (sort-by (comp - val) (:tools nums))] (str n " " t))) "."]
           [:h2 "Prompts"]
           [:table
            [:thead [:tr [:th "#"] [:th "what it is"] [:th "text"] [:th.n "steps"] [:th.n "tokens out"] [:th.n "cost"]]]
            [:tbody (for [r ps]
                      [:tr [:td [:a {:href (str "p/" (:n r) ".html")} (:n r)]] [:td (kind-tag (:kind r))] [:td (clip (:title r) 110)]
                       [:td.n (count (:work r))] [:td.n (if (:same-turn-as r) (str "same turn as " (:same-turn-as r)) (thou (:output_tokens (:usage r))))] [:td.n (usd (:cost r))]])]]
           (when (seq as)
             (list [:h2 "Subagent runs"]
                   [:table
                    [:thead [:tr [:th "run"] [:th "brief"] [:th.n "steps"] [:th.n "tokens out"] [:th.n "cost"]]]
                    [:tbody (for [r as]
                              [:tr [:td [:a {:href (str "a/" (:agent r) ".html")} (subs (:agent r) 0 (min 9 (count (:agent r))))]] [:td (clip (:title r) 110)]
                               [:td.n (count (:work r))] [:td.n (thou (:output_tokens (:usage r)))] [:td.n (usd (:cost r))]])]]))
           [:h2 "How to read it"]
           [:p "Every prompt, every tool call and every end of a turn is one commit on the session's branch in the trace repository; a subagent has a branch of its own, cut from the session when it starts. "
            "A commit's tree is the whole project right after the call, so the diff of a commit is what that call changed and a read changes nothing. "
            "The message says which tool ran and what it cost; what the model said and the raw tool call hang on the commit as git notes. "
            "A subagent's report does not come back as a git merge: the next prompt of the session carries it, and the end-of-turn commit names the agent's last commit in an " [:code "Agent-Report"] " trailer."]
           [:h2 "What was left out"]
           [:p "Every commit carries a snapshot of the author's instruction files and, where the project has them, of its memory files (the .claude-trace folder of every commit). None of it is in these pages; only the number of files in each tree is."
            (if (seq cuts)
              (list " " (str (reduce + 0 (vals cuts)) " pieces of text (counted once each, wherever they occur) were cut out and replaced by a marker: ")
                    (str/join "; " (for [[why n] cuts] (str n " × " why))) ".")
              " Nothing else was cut.")]
           [:p.src "Source: the branches of session " [:code (:session data)] " of the reus trace repository, read only."])
          "")))

;; --- graph.json ------------------------------------------------------------------------

(defn graph-data
  "The data a picture of the session needs, texts cut to the caps."
  [data runs nums]
  (let [clipn (fn [s n] (let [[t more] (cut s n)] (cond-> {:text t} (pos? more) (assoc :more more :chars (count (str s))))))
        agent-pages (into {} (for [l (:links data)] [(:agent l) (str "a/" (:agent l) ".html")]))
        page-of (fn [c] (if (:agent c) (str "a/" (:agent c) ".html#c" (:short c)) (str "p/" (:group c) ".html#c" (:short c))))]
    {:session (:session data) :project (:project data)
     :branches (:branches data)
     :links (:links data)
     :numbers (dissoc nums :t0 :t1)
     :span-seconds (- (:t1 nums) (:t0 nums))
     :prompts (vec (for [r runs]
                     {:id (:id r) :agent (:agent r) :n (:n r) :kind (:kind r) :title (:title r) :commits (mapv :sha (:commits r))
                      :steps (count (:work r)) :tools (:tools r) :usage (:usage r) :cost (:cost r) :model (:model r)
                     :usage-partial (:usage-partial? r) :same-turn-as (:same-turn-as r)}))
     :commits (vec (for [c (:commits data)]
                     (cond-> {:sha (:sha c) :short (:short c) :branch (:branch c) :agent (:agent c) :group (:group c) :kind (:kind c)
                              :at (:at c) :subject (:subject c) :parents (:parents c)
                              :message (:text (clipn (strip-trailers (:body c)) 1500)) :message-more (:more (clipn (strip-trailers (:body c)) 1500))
                              :trailers (:trailers c) :tool (:tool c)
                              :files (:files c) :add (:add c) :del (:del c)
                              :tree-files (:tree-files c)
                              :notes (into {} (for [[k v] (:notes c)] [k (clipn v *cap-note*)]))
                              :page (page-of c)}
                       (:diff c) (assoc :diff (clipn (:diff c) *cap-diff*))
                       (= :prompt (:base c)) (assoc :prompt-text (clipn (:text c) 1200))
                       (= :end (:kind c)) (assoc :reply (clipn (:text c) 1200)))))}))

;; --- the export ------------------------------------------------------------------------

(defn- distinct-counts [m] (into {} (map (fn [[k v]] [k (count v)])) m))

(defn export!
  "Write the export of session8 into dir. opts {:redact rules}. Returns {:files n :bytes n :cuts {why n} :commits n}."
  [sid8 dir & [{:keys [redact]}]]
  (let [[red counts] (redactor redact)
        data (walk-strings red (collect sid8))
        runs (runs-of data)
        nums (numbers data runs)
        agent-pages (into {} (for [l (:links data)]
                               [(:fork-from l) (str "a/" (:agent l) ".html")]))
        ps (vec (remove :agent runs))
        dir (str dir)]
    (doseq [d ["p" "a"]] (fs/delete-tree (fs/path dir d)) (fs/create-dirs (fs/path dir d)))
    (doseq [[i r] (map-indexed vector runs)]
      (let [prev (when-not (:agent r) (let [j (.indexOf ps r)] (when (pos? j) (nth ps (dec j)))))
            nxt (when-not (:agent r) (let [j (.indexOf ps r)] (when (< (inc j) (count ps)) (nth ps (inc j)))))]
        (spit (str (fs/path dir (if (:agent r) "a" "p") (str (if (:agent r) (:agent r) (:n r)) ".html")))
              (run-page r data agent-pages
                        (when-not (:agent r)
                          (list (when prev [:a {:href (str (:n prev) ".html")} (str "‹ prompt " (:n prev))]) "  "
                                (when nxt [:a {:href (str (:n nxt) ".html")} (str "prompt " (:n nxt) " ›")])))))))
    (spit (str (fs/path dir "index.html")) (index-page data runs nums (distinct-counts @counts)))
    (spit (str (fs/path dir "graph.json")) (json/generate-string (graph-data data runs nums)))
    (let [fs* (filter fs/regular-file? (fs/glob dir "**"))]
      {:files (count fs*) :bytes (reduce + (map fs/size fs*)) :cuts (distinct-counts @counts) :commits (count (:commits data)) :prompts (count ps) :agents (count (filter :agent runs))})))

(defn -main
  "bb trace:export SESSION DIR [--repo DIR] [--redact FILE] [--cap-diff N] [--cap-note N]"
  [& args]
  (let [[pos opts] (loop [[a & more :as xs] args pos [] o {}]
                     (cond (empty? xs) [pos o]
                           (str/starts-with? a "--") (recur (rest more) pos (assoc o (subs a 2) (first more)))
                           :else (recur more (conj pos a) o)))
        [sid dir] pos]
    (when (or (str/blank? sid) (str/blank? dir))
      (println "usage: bb trace:export SESSION DIR [--repo DIR] [--redact FILE] [--cap-diff N] [--cap-note N]") (System/exit 2))
    (binding [g/*repo* (or (get opts "repo") g/*repo*)
              *cap-diff* (or (some-> (get opts "cap-diff") parse-long) *cap-diff*)
              *cap-note* (or (some-> (get opts "cap-note") parse-long) *cap-note*)]
      (let [r (export! (subs sid 0 (min 8 (count sid))) dir {:redact (some-> (get opts "redact") slurp edn/read-string)})]
        (println (format "exported session %s: %d commits, %d prompts, %d subagent runs -> %s (%d files, %.1f MB)"
                         sid (:commits r) (:prompts r) (:agents r) dir (:files r) (/ (:bytes r) 1048576.0)))
        (doseq [[why n] (:cuts r)] (println (format "  cut %d x %s" n why)))))))
