(ns trace.cost
  "What building a project cost, counted from what was recorded. Read only: nothing here writes to a trace
  repo, a transcript folder or a git repo.

    bb trace:cost --method FILE [--out FILE]    count, as FILE says (an edn map, see below), write the result
    bb trace:cost --from RESULT [--out FILE]    count again with the :method a result carries

  Two sources, and the order between them is the point:
  - TRANSCRIPTS (Claude Code's own ~/.claude/projects, the main sessions and their subagent runs) are the
    primary source: every message with its model, its cwd and its usage, the cache write split into the
    5-minute and the 1-hour part, so the cost at list price is exact (trace.runs/cost, the CLI's own figure).
    A message belongs to the project its own cwd is in; a message in no project (a session started in the
    folder above all of them) is counted apart as :elsewhere, never into a project. Message ids are counted once.
  - TRACES (reus's repos) are the cross-check and fill in what no transcript has: per branch (session or
    agent run) the prompt, response and tool commits with their Model and Tokens trailers; a branch belongs
    to the project its root folder is in. :gap says, per project, how much of the transcripts' tokens the
    traces hold (they miss the end of a turn written after the Stop hook, before the fix of 2026-11-05, and
    all sessions before reus existed), :only-in-traces what a trace has and no transcript.
  Git (the shared remote repos) gives commits, lines added and removed, days. What is in none of them is said in
  :not-recorded (git commits on days with no recorded session, commits before the first record).

  The method (a map, stored in every result):
    {:zone \"Europe/Bucharest\"            the day boundary
     :transcripts [dir ...]               ~/.claude/projects folders (read only)
     :traces [{:name :repo} ...]          reus trace repos (read only)
     :projects {name {:roots [folder ...] :git [bare-repo ...] :counted? true}}}   roots: where its sessions ran"
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [jsonl.read :as jr]
            [trace.git :as g]
            [trace.read :as r]
            [trace.runs :as runs])
  (:import [java.time Instant ZoneId]))

;; --- small things ------------------------------------------------------------------------------------

(defn- zone [m] (ZoneId/of (or (:zone m) "UTC")))
(defn- day-of-ts [ts z] (str (.toLocalDate (.atZone (Instant/parse ts) z))))
(defn- day-of-secs [secs z] (str (.toLocalDate (.atZone (Instant/ofEpochSecond secs) z))))
(defn- under? [p root] (or (= p root) (str/starts-with? p (str root "/"))))

(defn project-of
  "The project a folder belongs to ({name {:roots [...]}}), nil for none."
  [projects cwd]
  (when cwd (some (fn [[n {:keys [roots]}]] (when (some #(under? cwd %) roots) n)) projects)))

(defn- usage-map
  "A usage in the keys trace.runs/cost reads; the 1-hour part of the cache write only when the usage says."
  [u]
  (let [cw (or (:cache_creation_input_tokens u) 0)]
    {:input_tokens (or (:input_tokens u) 0) :output_tokens (or (:output_tokens u) 0)
     :cache_read_input_tokens (or (:cache_read_input_tokens u) 0)
     :cache_creation_input_tokens cw
     :cache_1h_input_tokens (min cw (or (get-in u [:cache_creation :ephemeral_1h_input_tokens]) 0))}))

(def ^:private zero-usage (usage-map {}))
(defn- add-usage [a b] (merge-with + a b))

(defn- tally
  "Add one message's usage to {:messages n :usage {...}}."
  [t u] (-> (or t {:messages 0 :usage zero-usage}) (update :messages inc) (update :usage add-usage u)))

(defn- add-tally [a b] {:messages (+ (:messages a 0) (:messages b 0)) :usage (add-usage (or (:usage a) zero-usage) (or (:usage b) zero-usage))})

;; --- transcripts -------------------------------------------------------------------------------------

(defn- scan-file
  "One transcript: {:session :agent :msgs {id {:ts :cwd :model :usage :tools #{}}} :prompts [{:ts :cwd}]}.
  A message spans several lines with the same id: the last line's usage, the first one's time."
  [f]
  (let [agent? (jr/agent-file? f)
        name* (fn [x] (str (fs/strip-ext (fs/file-name x))))]
    (with-open [rd (io/reader (str f))]
      (assoc (reduce
              (fn [st l]
                (cond
                  (str/includes? l "\"type\":\"assistant\"")
                  (let [e (try (json/parse-string l true) (catch Exception _ nil))
                        m (:message e) id (:id m)]
                    (if (and e (= "assistant" (:type e)) id)
                      (let [cwd (or (:cwd e) (:cwd st))
                            tools (into #{} (keep #(when (= "tool_use" (:type %)) (:id %))) (when (sequential? (:content m)) (:content m)))]
                        (-> st (assoc :cwd cwd)
                            (update-in [:msgs id] (fn [o] {:ts (or (:ts o) (:timestamp e)) :cwd (or (:cwd o) cwd) :model (:model m)
                                                           :usage (:usage m) :tools (into (or (:tools o) #{}) tools)}))))
                      st))

                  (and (str/includes? l "\"type\":\"user\"") (not (str/includes? l "\"tool_result\"")))
                  (let [e (try (json/parse-string l true) (catch Exception _ nil))]
                    (if (and e (= "user" (:type e)) (jr/prompt-text e))
                      (let [cwd (or (:cwd e) (:cwd st))]
                        (-> st (assoc :cwd cwd) (update :prompts conj {:ts (:timestamp e) :cwd cwd})))
                      st))
                  :else st))
              {:cwd nil :msgs {} :prompts []} (line-seq rd))
             :agent (when agent? (str/replace (name* f) #"^agent-" ""))
             :session (if agent? (name* (fs/parent (fs/parent f))) (name* f))))))

(defn- fresh-acc [] {:projects {} :elsewhere nil :sess {} :seen #{} :files 0})

(defn- add-file
  "Fold a scanned transcript into acc; a message id seen before (a resumed session copies its past) is skipped."
  [acc {:keys [session agent msgs prompts]} projects z]
  (let [skey (str (subs session 0 (min 8 (count session))) (when agent (str "-" agent)))
        acc (update acc :files inc)
        acc (reduce
             (fn [acc [id {:keys [ts cwd model usage tools]}]]
               (if (or (contains? (:seen acc) id) (= "<synthetic>" model))
                 acc
                 (let [u (usage-map usage) pr (project-of projects cwd) day (when ts (day-of-ts ts z))]
                   (cond-> (update acc :seen conj id)
                     (nil? pr) (update :elsewhere (fn [e] (-> e (update :by-model update model tally u) (update :tool-calls (fnil + 0) (count tools)))))
                     pr (update-in [:projects pr] (fn [pj]
                                                    (-> (or pj {})
                                                        (update :by-model update model tally u)
                                                        (update :tool-calls (fnil + 0) (count tools))
                                                        (update :days (fnil conj #{}) day)
                                                        (update :sessions (fnil conj #{}) session)
                                                        (update :agents (fn [a] (cond-> (or a #{}) agent (conj [session agent]))))
                                                        (update :first (fn [o] (if (or (nil? o) (neg? (compare ts o))) ts o)))
                                                        (update :last (fn [o] (if (or (nil? o) (pos? (compare ts o))) ts o))))))
                     pr (update-in [:sess [skey pr]] (fn [s] (-> (or s {:agent? (boolean agent)}) (update :tally add-tally {:messages 1 :usage u}))))))))
             acc msgs)]
    (reduce (fn [acc {:keys [ts cwd]}]
              (if-let [pr (project-of projects cwd)]
                (-> acc (update-in [:projects pr :prompts] (fnil inc 0)) (update-in [:projects pr :days] (fnil conj #{}) (day-of-ts ts z))
                    (update-in [:sess [skey pr] :prompts] (fnil inc 0)))
                (update-in acc [:elsewhere :prompts] (fnil inc 0))))
            acc prompts)))

(defn- relevant?
  "Does any line of the transcript have a cwd in one of the roots? (the quick look before the real parse)"
  [f needles]
  (with-open [rd (io/reader (str f))]
    (boolean (some (fn [l] (some #(str/includes? l %) needles)) (line-seq rd)))))

(defn scan-transcripts
  "Fold every transcript under the given project folders (main sessions and subagent runs) that has a message
  in a project's roots."
  [dirs projects z]
  (let [needles (vec (for [[_ {:keys [roots]}] projects r roots] (str "\"cwd\":\"" r)))
        files (->> dirs (mapcat #(fs/glob % "**.jsonl")) (map str) sort (filter #(relevant? % needles)))]
    (reduce (fn [acc f] (try (add-file acc (scan-file f) projects z)
                             (catch Exception e (binding [*out* *err*] (println "skip" f (ex-message e))) acc)))
            (fresh-acc) files)))

;; --- traces ------------------------------------------------------------------------------------------

(def ^:private RS "\u001e")
(def ^:private US "\u001f")

(defn- branch-records
  "[{:sha :at :subject :body}] of a branch's own commits, oldest first, without numstat."
  [tip agent?]
  (let [shas (map :sha (reverse (r/own-commits tip agent?)))]
    (when (seq shas)
      (->> (str/split (g/git "log" "--no-walk=unsorted" "--stdin" (str "--format=%H" US "%at" US "%s" US "%b" RS)
                             {:in (str (str/join "\n" shas) "\n")})
                      (re-pattern RS))
           (remove str/blank?)
           (map #(let [[sha at s b] (str/split (str/triml %) (re-pattern US) -1)] {:sha sha :at (parse-long at) :subject s :body (or b "")}))))))

(defn- trailer-usage [tr]
  (let [n (fn [k] (or (some-> (get tr k) parse-long) 0))]
    {:input_tokens (n "Tokens-In") :output_tokens (n "Tokens-Out") :cache_read_input_tokens (n "Cache-Read")
     :cache_creation_input_tokens (n "Cache-Write") :cache_1h_input_tokens 0}))

(defn branch-facts
  "One session or agent branch of a trace: {:root :prompts :tools :days :by-model} from its commits. A prompt's
  tokens are its response commit's (the sum of the turn), else those of its tool commits; its model the first
  tool commit's."
  [recs z]
  (let [root (some #(second (re-find #"(?m)^root (.+)$" (:body %))) recs)
        by-prompt (reduce (fn [m {:keys [subject body at]}]
                            (let [[_ tr] (r/split-trailers body)
                                  [_ pn] (re-find #"^prompt ([\w-]+):" subject)
                                  [_ rn] (re-matches #"response ([\w-]+)" subject)
                                  [_ tn] (re-find #"^([\w-]+)\.\d+ " subject)]
                              (cond pn (assoc-in m [pn :prompt] true)
                                    rn (assoc-in m [rn :response] (trailer-usage tr))
                                    tn (cond-> (update-in m [tn :tools] (fnil inc 0))
                                         (get tr "Model") (update-in [tn :model] #(or % (get tr "Model")))
                                         (get tr "Tokens-Out") (update-in [tn :tool-usage] (fnil add-usage zero-usage) (trailer-usage tr)))
                                    :else m)))
                          {} recs)]
    {:root root
     :prompts (count (filter (comp :prompt val) by-prompt))
     :tools (reduce + (map #(:tools % 0) (vals by-prompt)))
     :days (into #{} (map #(day-of-secs (:at %) z)) recs)
     :by-model (reduce (fn [m [_ {:keys [response tool-usage model]}]]
                         (let [u (or response tool-usage)] (if u (update m (or model "unknown") tally u) m)))
                       {} by-prompt)}))

(defn scan-traces
  "Fold the session and agent branches of one trace repo whose root is in a project."
  [repo projects z]
  (binding [g/*repo* repo]
    (let [tips (r/tips)]
      (reduce
       (fn [acc [b tip]]
         (if-let [[_ _ kind suf] (re-matches #"(.+)/([sa])/([^/]+)" b)]
           (let [f (branch-facts (branch-records tip (= kind "a")) z)
                 pr (project-of projects (:root f))]
             (if (and pr (pos? (:prompts f)))
               (let [skey (if (= kind "s") suf (let [[s a] (str/split suf #"-" 2)] (str s "-" a)))]
                 (-> acc
                     (update-in [:projects pr :branches] (fnil conj []) b)
                     (update-in [:projects pr :prompts] (fnil + 0) (:prompts f))
                     (update-in [:projects pr :tool-calls] (fnil + 0) (:tools f))
                     (update-in [:projects pr :days] (fnil into #{}) (:days f))
                     (update-in [:projects pr :by-model] #(merge-with add-tally (or % {}) (:by-model f)))
                     (update-in [:sess [skey pr]] (fn [s] {:agent? (= kind "a") :branch b
                                                           :prompts (:prompts f) :tally (reduce add-tally {} (vals (:by-model f)))}))))
               acc))
           acc))
       {:projects {} :sess {}} tips))))

;; --- git ---------------------------------------------------------------------------------------------

(defn git-stats
  "Commits, lines added and removed, first and last day, the days with a commit, of a bare or ordinary repo (all refs)."
  [repo z]
  (let [out (:out (p/shell {:out :string :err :string :continue true} "git" "-C" (str repo) "log" "--all" "--numstat"
                           (str "--format=" "\u001f%H\u001f%at")))
        [commits added deleted days]
        (reduce (fn [[c a d ds] l]
                  (cond (str/starts-with? l "\u001f") (let [[_ _ at] (str/split l #"\u001f")] [(inc c) a d (conj ds (day-of-secs (parse-long at) z))])
                        (re-matches #"(\d+)\t(\d+)\t.*" l) (let [[_ x y] (re-matches #"(\d+)\t(\d+)\t.*" l)] [c (+ a (parse-long x)) (+ d (parse-long y)) ds])
                        :else [c a d ds]))
                [0 0 0 []] (str/split-lines out))]
    {:commits commits :added added :deleted deleted :days (vec (sort (distinct days)))
     :first (first (sort days)) :last (last (sort days))
     :by-day (frequencies days)}))

;; --- the result --------------------------------------------------------------------------------------

(defn- priced
  "{model {:messages :usage :cost}} (cost nil for a model the price table lacks)."
  [by-model]
  (into (sorted-map) (map (fn [[m t]] [m (assoc t :cost (runs/cost {:model m :usage (:usage t)}))])) by-model))

(defn- sum-cost [by-model] (reduce + 0.0 (keep :cost (vals by-model))))
(defn- sum-tally [by-model] (reduce add-tally {} (vals by-model)))

(defn- tokens [t]
  (let [u (or (:usage t) zero-usage)]
    {:in-fresh (:input_tokens u) :out (:output_tokens u) :cache-read (:cache_read_input_tokens u)
     :cache-write (:cache_creation_input_tokens u) :cache-write-1h (:cache_1h_input_tokens u)
     :in-total (+ (:input_tokens u) (:cache_read_input_tokens u) (:cache_creation_input_tokens u))}))

(defn- project-row [{:keys [days sessions agents prompts tool-calls by-model first last]} z]
  (let [pm (priced by-model)]
    {:sessions (count sessions) :agent-runs (count agents) :prompts (or prompts 0) :tool-calls (or tool-calls 0)
     :messages (:messages (sum-tally by-model) 0) :tokens (tokens (sum-tally by-model))
     :cost-usd (sum-cost pm) :unpriced (vec (sort (map key (filter #(nil? (:cost (val %))) pm))))
     :by-model (into (sorted-map) (map (fn [[m t]] [m (merge {:messages (:messages t)} (tokens t) {:cost-usd (:cost t)})])) pm)
     :days (count days) :first-day (some-> first (day-of-ts z)) :last-day (some-> last (day-of-ts z))
     :active-days (vec (sort days))}))

(defn- gap
  "Transcripts against traces for one project: sessions both have, only transcripts, only traces; the share of the
  transcripts' output and cache-read tokens (of the sessions both have) the traces hold."
  [tsess ssess pr]
  (let [of (fn [m] (into {} (filter (fn [[[_ p] _]] (= p pr))) m))
        t (of tsess) s (of ssess)
        both (filter #(contains? s %) (keys t))
        sum (fn [m ks k] (reduce + (map #(get-in m [% :tally :usage k] 0) ks)))]
    {:sessions-both (count both)
     :sessions-only-in-transcripts (count (remove #(contains? s %) (keys t)))
     :sessions-only-in-traces (count (remove #(contains? t %) (keys s)))
     :of-the-sessions-both {:out {:transcripts (sum t both :output_tokens) :traces (sum s both :output_tokens)}
                            :cache-read {:transcripts (sum t both :cache_read_input_tokens) :traces (sum s both :cache_read_input_tokens)}
                            :prompts {:transcripts (reduce + (map #(get-in t [% :prompts] 0) both)) :traces (reduce + (map #(get-in s [% :prompts] 0) both))}}
     :out-share-in-traces (let [a (sum t both :output_tokens)] (when (pos? a) (double (/ (sum s both :output_tokens) a))))}))

(defn- only-in-traces [tsess ssess pr]
  (let [ks (filter (fn [[_ p :as k]] (and (= p pr) (not (contains? tsess k)))) (keys ssess))
        tl (reduce add-tally {} (map #(get-in ssess [% :tally]) ks))]
    {:sessions (count ks) :tokens (tokens (if (seq ks) tl {:usage zero-usage}))}))

(defn- not-recorded [gits days first-day]
  (let [by-day (apply merge-with + (map :by-day gits))
        dayset (set days)]
    {:commits-before-first-recorded-day (when first-day (reduce + (map val (filter #(neg? (compare (key %) first-day)) by-day))))
     :commits-on-days-with-no-recorded-session (reduce + (map val (remove #(contains? dayset (key %)) by-day)))
     :days-with-commits-and-no-session (count (remove dayset (keys by-day)))}))

(defn count-all
  "The result map for a method (see ns)."
  [method]
  (let [z (zone method) projects (:projects method)
        t (scan-transcripts (:transcripts method) projects z)
        stores (for [s (:traces method)] (assoc (scan-traces (:repo s) projects z) :name (:name s)))
        ssess (apply merge (map :sess stores))
        rows (into (sorted-map)
                   (for [[pr cfg] projects]
                     (let [row (project-row (get-in t [:projects pr] {}) z)
                           gits (mapv #(assoc (git-stats % z) :repo (str (fs/file-name %))) (:git cfg))
                           pdays (get-in t [:projects pr :days])
                           trace-days (into #{} (mapcat #(get-in % [:projects pr :days])) stores)
                           all-days (into (set pdays) trace-days)
                           first-day (first (sort all-days))]
                       [pr (merge row
                                  {:counted? (:counted? cfg true)
                                   :git {:repos (mapv #(dissoc % :by-day :days) gits)
                                         :commits (reduce + (map :commits gits)) :added (reduce + (map :added gits))
                                         :deleted (reduce + (map :deleted gits))
                                         :first-day (first (sort (keep :first gits))) :last-day (last (sort (keep :last gits)))
                                         :days-with-commits (count (into #{} (mapcat :days) gits))}
                                   :traces {:sessions (count (filter (fn [[_ p]] (= p pr)) (keys ssess)))
                                            :prompts (reduce + (map #(get-in % [:projects pr :prompts] 0) stores))
                                            :tool-calls (reduce + (map #(get-in % [:projects pr :tool-calls] 0) stores))
                                            :tokens (tokens (sum-tally (apply merge-with add-tally (map #(get-in % [:projects pr :by-model] {}) stores))))
                                            :days (count trace-days)}
                                   :gap (gap (:sess t) ssess pr)
                                   :only-in-traces (only-in-traces (:sess t) ssess pr)
                                   :not-recorded (not-recorded gits all-days first-day)})])))
        counted (filter :counted? (vals rows))
        total (fn [k] (reduce + (map k counted)))
        by-model (apply merge-with (fn [a b] (merge-with #(+ (or %1 0) (or %2 0)) a b)) (map #(into {} (map (fn [[m x]] [m (select-keys x [:messages :out :in-fresh :cache-read :cache-write :cache-write-1h :cost-usd])])) (:by-model %)) counted))]
    {:generated (str (Instant/now)) :method method
     :projects rows
     :total {:projects (vec (keep (fn [[k v]] (when (:counted? v) k)) rows))
             :sessions (total :sessions) :agent-runs (total :agent-runs) :prompts (total :prompts) :tool-calls (total :tool-calls)
             :messages (total :messages) :cost-usd (total :cost-usd) :by-model (into (sorted-map) by-model)
             :commits (reduce + (map #(get-in % [:git :commits]) counted))}
     :elsewhere (let [e (:elsewhere t) pm (priced (:by-model e))]
                  {:what "messages of sessions in no project folder (started in the folder above all projects): not counted into any project"
                   :prompts (:prompts e 0) :tool-calls (:tool-calls e 0) :messages (:messages (sum-tally (:by-model e)) 0)
                   :tokens (tokens (sum-tally (:by-model e))) :cost-usd (sum-cost pm)
                   :by-model (into (sorted-map) (map (fn [[m x]] [m (merge {:messages (:messages x)} (tokens x) {:cost-usd (:cost x)})])) pm)})
     :transcripts {:files (:files t) :message-ids (count (:seen t))}}))

;; --- command line ------------------------------------------------------------------------------------

(defn -main [& args]
  (let [opts (apply hash-map (map #(if (str/starts-with? % "--") (keyword (subs % 2)) %) args))
        method (cond (:method opts) (edn/read-string (slurp (:method opts)))
                     (:from opts) (:method (edn/read-string (slurp (:from opts))))
                     :else (do (println "usage: bb trace:cost --method FILE | --from RESULT [--out FILE]") (System/exit 2)))
        res (count-all method)
        text (with-out-str (pp/pprint res))]
    (if (:out opts) (spit (:out opts) text) (print text))))
