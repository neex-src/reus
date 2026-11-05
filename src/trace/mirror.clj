(ns trace.mirror
  "The viewer's pages for the nod mirrors of oglinda: a real Claude session next to its twin, prompt by prompt.
  A mirror re-runs a real session's prompts in a sandbox with nod (a code tool that reads and edits like an IDE)
  instead of search and replace, so the two can be compared.

    /mirrors                        every mirrored session: cost, time, API calls, nod calls and failures, guard
    /mirror/<id>[?only=failures]    one session: the verdict in a sentence, the totals, each prompt with the real
                                    side next to the twin, the result per repo, nod's own calls and failures
    /mirror/<id>/p/<n>[?show=nod|failed]   one prompt: every call of both sides, expandable

  Plain pages (trace.page): no script, no CDN, no box that scrolls, no text over text. A verdict is computed from
  the numbers and says what changed, never which side is better.

  oglinda is found at run time, like trace.ash finds ashamer, and is NOT in bb.edn :deps: bb resolves those
  for every task, the hook's too, and the recording path (hook, worker, record) must never need oglinda.
  Where it is: OGLINDA_HOME when set (then only there), else `oglinda` beside this reus checkout, else
  ~/projects/oglinda (the sandbox box), else ~/projects/oglinda. Absent, only these pages say so, and where they
  looked; nothing else notices. This namespace never writes into oglinda: what oglinda would write while
  comparing (its diff.git) is made here, in var/mirror-diff of this checkout.

  The numbers are oglinda.compare's own (side, mirror-turns, real-turns, with-cost: one definition of busy,
  wall, cost, nod call and failure for both sides). What this namespace adds is the per-call detail
  `compare` counts and throws away: the code sent, nod's reply, how long it took."
  (:require [babashka.classpath :as cp]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [trace.git :as g]
            [trace.page :as pg :refer [thou clip]])
  (:import [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

;; --- oglinda, found at run time ------------------------------------------------

(defn candidates
  "Where oglinda may be, in the order it is looked for: [dir ...]. env: OGLINDA_HOME or nil (set: the only place),
  reus-home: this checkout, user-home: ~."
  [env reus-home user-home]
  (if (not (str/blank? env))
    [env]
    (vec (distinct [(str (fs/path (fs/parent reus-home) "oglinda"))
                    (str (fs/path user-home "prog" "oglinda"))
                    (str (fs/path user-home "projects" "oglinda"))]))))

(defn- has-oglinda? [dir] (fs/exists? (fs/path dir "src" "oglinda" "compare.clj")))

(defn- looked-in [] (candidates (System/getenv "OGLINDA_HOME") g/home (System/getProperty "user.home")))

(defn home
  "The oglinda checkout: the first of `candidates` that has it; when none has, the first one (the pages say where
  they looked)."
  []
  (let [cs (looked-in)] (or (first (filter has-oglinda? cs)) (first cs))))

(defn available? [] (boolean (some has-oglinda? (looked-in))))

(defonce ^:private loaded (atom false))

(defn load!
  "Put oglinda on the classpath (once; its src and its resources, where config.edn is); throws ex-info
  {:oglinda :missing} when it is not there."
  []
  (when-not @loaded
    (when-not (available?)
      (throw (ex-info (str "oglinda is not installed. Looked in " (str/join ", " (looked-in))
                           ". OGLINDA_HOME says where it is.")
                      {:oglinda :missing})))
    (cp/add-classpath (str (fs/path (home) "src")))
    (cp/add-classpath (str (fs/path (home) "resources")))
    (require 'oglinda.compare 'oglinda.state 'oglinda.config 'oglinda.transcript 'oglinda.repin 'oglinda.around 'oglinda.world)
    (reset! loaded true)))

(defn- f
  "The var of oglinda's fully qualified symbol sym, loading oglinda first."
  [sym]
  (load!)
  (requiring-resolve sym))

(defn- missing? [e] (= :missing (:oglinda (ex-data e))))

;; --- what a side did, call by call ----------------------------------------------

(def ^:private input-long-keys #{"content" "code" "old_string" "new_string" "new_content" "command" "prompt"})

(defn- first-line [s] (first (str/split-lines (str s))))

(defn- call-label [{:keys [name input]}]
  (let [code (or (:code input) (:command input))
        target (or (:file_path input) (:path input))]
    (cond
      code (clip (first-line code) 150)
      target (str target (when-let [p (or (:pattern input) (:old_string input))] (str "  ·  " (clip (first-line p) 60))))
      (= "Agent" name) (str (:description input))
      :else (clip (or (:pattern input) (:query input) (:url input) (pr-str input)) 120))))

(defn- sidechain-ids
  "Ids of the tool calls the subagents made (their lines carry :isSidechain)."
  [ls]
  (set (for [l ls :when (and (:isSidechain l) (= "assistant" (:type l)))
             b (let [c (get-in l [:message :content])] (when (sequential? c) c))
             :when (= "tool_use" (:type b))]
         (:id b))))

(defn call-list
  "The tool calls of some transcript lines in order, each as {:i :name :input :label :nod? :failed :hung :refused
  :dur :text :sub? :error}. :text is the result; :failed is true for a nod call that failed (oglinda.compare's
  failed-output?) and for any other call whose result is an error; :error is what to show first (nod's own error
  line)."
  [ls]
  (let [calls (f 'oglinda.compare/calls)
        eval-call? (f 'oglinda.compare/eval-call?)
        nod-sub (f 'oglinda.compare/nod-sub)
        failed-output? (f 'oglinda.compare/failed-output?)
        failure-text (f 'oglinda.compare/failure-text)
        refused? (f 'oglinda.compare/refused-call?)
        hung-re @(f 'oglinda.compare/nod-hung-text)
        slow-s @(f 'oglinda.compare/slow-nod-s)
        side-ids (sidechain-ids ls)
        last-at (some->> ls (keep #(some-> (:timestamp %) Instant/parse .toEpochMilli)) seq (apply max))]
    (vec (map-indexed
          (fn [i c]
            (let [nod? (boolean (or (eval-call? c) (and (= "Bash" (:name c)) (nod-sub (get-in c [:input :command])))))
                  res (:result c)
                  text (str (:text res))
                  dur (when (:at c) (some-> (or (:at res) last-at) (- (:at c)) (/ 1000.0)))
                  hung (boolean (and nod? (or (and res (re-find hung-re text)) (and (nil? res) dur (> dur slow-s)))))
                  refused (boolean (refused? c))
                  failed (boolean (or hung (if nod? (and res (failed-output? res)) (and (:error? res) (not refused)))))]
              (-> c
                  (assoc :i (inc i) :nod? nod? :text text :dur dur :hung hung :failed failed :refused refused
                         :sub? (contains? side-ids (:id c)) :pending (nil? res)
                         :error (cond hung (if (str/blank? text) "no reply: the call never got an answer" (failure-text text))
                                      (and failed nod?) (failure-text text)
                                      failed (clip text 300)))
                  (assoc :label (call-label (assoc c :nod? nod?)))
                  (dissoc :result))))
          (sort-by #(or (:at %) 0) (calls ls))))))

;; --- the words of a comparison ----------------------------------------------------

(defn- ms-of [ts] (try (.toEpochMilli (Instant/parse ts)) (catch Exception _ nil)))

(defn- turn-start [ls] (some->> ls (keep #(ms-of (:timestamp %))) seq (apply min)))

(defn- pct [m r] (when (and m r (pos? r)) (* 100.0 (/ (- m r) r))))
(defn- pct-str [p] (when p (if (< (Math/abs (double p)) 0.5) "±0%" (str/replace (format "%+.0f%%" p) "-" "−"))))

(defn- usd [x] (if x (format "$%.2f" (double x)) "–"))
(defn- dur-str [s]
  (when s
    (let [s (Math/round (double s))]
      (cond (>= s 3600) (format "%dh %02dm" (quot s 3600) (quot (mod s 3600) 60))
            (>= s 60) (format "%dm %02ds" (quot s 60) (mod s 60))
            :else (str s "s")))))
(defn- short-dur [s] (when s (if (< s 10) (format "%.1fs" (double s)) (dur-str s))))
(defn- s7 [x] (some-> x str (subs 0 (min 7 (count (str x))))))
(defn- stamp-str [ms] (when ms (.format (DateTimeFormatter/ofPattern "MMM d, HH:mm") (.atZone (Instant/ofEpochMilli ms) (ZoneId/systemDefault)))))

(defn- n-of "n with its noun, one or many: 1 call, 2 calls." [n one many] (str (thou n) " " (if (= 1 n) one many)))
(defn- err-word [x] (if (keyword? x) (name x) (str x)))
(defn- err? [x] (and x (not (#{"" "success"} (err-word x)))))

(defn turn-notes
  "What went wrong with a prompt's turn on either side, in words: said before any comparison."
  [real mirror]
  (cond-> []
    (not real) (conj "The real session has no transcript for this prompt.")
    (not mirror) (conj "The twin has not run this prompt.")
    (:interrupted real) (conj "The real session's turn was interrupted by the user.")
    (:interrupted mirror) (conj "The twin's turn was interrupted.")
    (err? (:error mirror)) (conj (str "The twin's turn ended with an error (" (err-word (:error mirror)) ")."))))

(defn comparison-text
  "The sentence for two sides (a prompt's, or sums over prompts) from their numbers: what the twin cost and how
  long it was busy against the real side. Neutral: it says more or less, never better or worse. nil when there is
  nothing to compare."
  [real mirror]
  (let [c (pct (:cost mirror) (:cost real))
        t (pct (:busy mirror) (:busy real))
        amount (fn [p unit] (str (if (< (Math/abs (double p)) 0.5)
                                   "about the same"
                                   (format "%.0f%% %s" (Math/abs (double p)) (if (neg? p) "less" "more")))
                                 (when unit (str " " unit))))]
    (cond
      (and c t) (str "The twin cost " (amount c nil) " (" (usd (:cost mirror)) " against " (usd (:cost real)) ") and was busy for "
                     (amount t "time") " (" (dur-str (:busy mirror)) " against " (dur-str (:busy real)) ").")
      c (str "The twin cost " (amount c nil) " (" (usd (:cost mirror)) " against " (usd (:cost real)) ").")
      t (str "The twin was busy for " (amount t "time") " (" (dur-str (:busy mirror)) " against " (dur-str (:busy real)) ").")
      :else nil)))

(defn verdict
  "A plain verdict on a prompt from its two sides: {:class :text}. What went wrong comes first, then what the
  twin cost and how long it was busy against the real side. :class is more | less | same (the cost) | none."
  [real mirror]
  (let [notes (turn-notes real mirror)
        both? (and real mirror)
        c (when both? (pct (:cost mirror) (:cost real)))
        text (when both? (comparison-text real mirror))]
    {:class (cond (nil? c) "none" (< (Math/abs (double c)) 0.5) "same" (neg? c) "less" :else "more")
     :text (str/join " " (concat notes (remove nil? [(or text (when both? "There is no cost to compare."))])))}))

;; --- the result per repo ----------------------------------------------------------

(defn- diff-repo!
  "A bare repo of this checkout (var/mirror-diff/<id>.git) whose alternates reach every object the numstat of a
  mirror's trees needs. oglinda keeps one of its own (diff.git) and writes into it while comparing: this
  namespace never does."
  [id plan]
  (let [d (fs/path g/home "var" "mirror-diff" (str id ".git"))
        mfile (f 'oglinda.state/mfile)
        layers (f 'oglinda.compare/world-layers)]
    (when-not (fs/exists? d)
      (fs/create-dirs (fs/parent d))
      (p/shell {:out :string :err :string} "git" "init" "-q" "--bare" (str d)))
    (spit (str (fs/path d "objects" "info" "alternates"))
          (str/join "\n" (concat [(mfile id "objects-own") (mfile id "record" "objects-own")]
                                 (layers id)
                                 (for [u (:units plan) o (:objects u)] (:real o)))))
    (str d)))

(defn- repo-results
  "What each side left in each repo (oglinda.compare/results, with the git dir of diff-repo!): {:recorded {:real :mirror}
  :repos [{:path :pairs [{:real :mirror :verdict ...}]}]}. oglinda takes each side's work when the side ends, for
  the session: there is no result per prompt."
  [id mm]
  (let [mfile (f 'oglinda.state/mfile)
        plan (or ((f 'oglinda.world/plan) (mfile id "world"))
                 ((f 'oglinda.config/read-edn) (mfile id "record" "plan.edn")))
        real (:real-work mm)
        mirror ((f 'oglinda.compare/mirror-work) id)
        numstat (f 'oglinda.compare/numstat)
        with-numstat (f 'oglinda.compare/with-numstat)
        pair-entries (f 'oglinda.compare/pair-entries)
        pair-verdict (f 'oglinda.compare/pair-verdict)]
    {:recorded {:real (some? real) :mirror (some? mirror)}
     :repos (when (and plan (or real mirror))
              (let [repo (diff-repo! id plan)]
                (vec (for [path (sort (distinct (concat (keys real) (keys mirror))))
                           :let [rs (map #(with-numstat repo %) (get real path))
                                 ms (map #(with-numstat repo %) (get mirror path))]]
                       {:path path
                        :pairs (vec (for [{r :real m :mirror :as pr} (pair-entries rs ms)
                                          :let [pr (assoc pr
                                                          :same-files (when (and r m) (= (set (:files r)) (set (:files m))))
                                                          :identical (boolean (and r m (:tree r) (= (:tree r) (:tree m))))
                                                          :between (when (and r m (:tree r) (:tree m) (not= (:tree r) (:tree m)))
                                                                     (numstat repo (:tree r) (:tree m))))]]
                                      (assoc pr :verdict (pair-verdict pr))))}))))}))

;; --- one session, analysed ------------------------------------------------------

(defn- queue-entries [id]
  (let [st-queued (f 'oglinda.state/queued) st-mfile (f 'oglinda.state/mfile) read-edn (f 'oglinda.config/read-edn)]
    (into (sorted-map) (for [n (st-queued id)] [n (read-edn (st-mfile id "queue" (format "%04d.edn" n)))]))))

(defn analyze
  "Everything the pages show of mirror id: the session's header and per prompt both sides (oglinda.compare's
  `side` plus cost), the verdict, the call lists of both, the guard's entries inside the prompt, and the result per
  repo."
  [id]
  (let [mm ((f 'oglinda.state/meta-of) id)
        cfg ((f 'oglinda.config/cfg))
        exts (get-in cfg [:nod :exts])
        prices (:prices cfg)
        side (f 'oglinda.compare/side)
        with-cost (f 'oglinda.compare/with-cost)
        events ((f 'oglinda.state/events) id)
        rt ((f 'oglinda.compare/real-turns) mm events)
        mt ((f 'oglinda.compare/mirror-turns) id)
        turns ((f 'oglinda.state/turns) id)
        nod-of-turn (f 'oglinda.repin/nod-of-turn)
        turn-nods (reduce (fn [m t] (if (and (:n t) (not (:background t)))
                                      (update m (:n t) (fn [xs] (let [x (nod-of-turn mm (:n t) t)] (if (or (nil? x) (some #{x} xs)) xs (conj (vec xs) x)))))
                                      m))
                          {} turns)
        {:keys [rows]} ((f 'oglinda.compare/split-entries) (queue-entries id) (when (seq rt) (set (keys rt))))
        guard ((f 'oglinda.compare/guard-record) id)
        starts (into {} (for [[n m] mt] [n (turn-start (:lines m))]))
        guard-owner (fn [{:keys [at]}] (some->> starts (filter (fn [[_ s]] (and s at (<= s at)))) (sort-by val) last key))
        guard-by (group-by guard-owner guard)
        prompts (vec (for [[n e] rows
                           :let [r (get rt (:prompt-id e)) mx (get mt n)
                                 real (when r (with-cost prices (side (:lines r) r exts)))
                                 mirror (when mx (assoc (with-cost prices (side (:lines mx) mx exts)) :error (:error mx) :injected (:injected mx)))]]
                       {:n n :text (str/join " " (keep :text (:content e)))
                        :nods (get turn-nods n)
                        :real real :mirror mirror
                        :verdict (verdict real mirror)
                        :real-calls (when r (call-list (:lines r)))
                        :twin-calls (when mx (call-list (:lines mx)))
                        :guard (vec (sort-by :at (get guard-by n)))}))]
    {:id id :cwd (:cwd mm) :created (:created mm) :state (:state mm) :reason (:reason mm) :first-prompt (:first-prompt mm)
     :nod (get-in mm [:tools :nod :sha]) :repins (:repins mm) :guard guard
     :prompts prompts
     :repos (try (repo-results id mm) (catch Exception e {:error (ex-message e)}))}))

(defn- sum-of [ps k]
  ((f 'oglinda.compare/sum-sides) ps k))

(defn- nod-calls-of [side] (+ (get-in side [:tools "nod-eval"] 0) (get-in side [:tools "nod"] 0)))

(defn summarize
  "A session for the list: the sums over its prompts, without the call lists."
  [{:keys [prompts] :as a}]
  (let [ps (map #(dissoc % :real-calls :twin-calls) prompts)
        real (sum-of ps :real) mirror (sum-of ps :mirror)
        both (filter #(and (:real %) (:mirror %)) ps)
        sum-cost (fn [k] (reduce + 0.0 (keep #(get-in % [k :cost]) both)))
        sum-busy (fn [k] (reduce + 0.0 (keep #(get-in % [k :busy]) both)))]
    (-> (select-keys a [:id :cwd :created :state :reason :nod :repins])
        (assoc :prompts (count prompts)
               :both (count both)
               ;; cost over the prompts both sides ran, so a half-run twin is not "cheaper"
               :real-cost (sum-cost :real) :twin-cost (sum-cost :mirror)
               :both-real-busy (sum-busy :real) :both-twin-busy (sum-busy :mirror)
               :real-calls (get-in real [:tokens :calls]) :twin-calls (get-in mirror [:tokens :calls])
               :real-busy (:busy real) :twin-busy (:busy mirror)
               :nod-calls (nod-calls-of mirror) :nod-failed (count (:nod-failed mirror))
               :nod-versions (vec (distinct (mapcat :nods prompts)))
               :real-interrupted (count (filter #(get-in % [:real :interrupted]) ps))
               :twin-interrupted (count (filter #(get-in % [:mirror :interrupted]) ps))
               :twin-errors (count (filter #(err? (get-in % [:mirror :error])) ps))
               :not-run (count (filter #(and (:real %) (not (:mirror %))) ps))
               :refused (count (filter #(= :refused (:decision %)) (:guard a)))
               :hatch (count (filter #(= :hatch (:decision %)) (:guard a)))))))

;; --- cache ----------------------------------------------------------------------

(defn- stamp
  "What changes when the session's data does: the sizes and times of the files the analysis reads."
  [id]
  (let [mst (f 'oglinda.state/mfile)
        mm ((f 'oglinda.state/meta-of) id)
        fs* (concat [(mst id "meta.edn") (mst id "events.edn") (mst id "record" "turns.edn") (mst id "record" "guard.edn") (:transcript mm)]
                    (try (map str (fs/glob (mst id "record") "stream-*.jsonl")) (catch Exception _ [])))]
    (mapv #(when (and % (fs/exists? %)) [(fs/size %) (.toMillis (fs/last-modified-time %))]) fs*)))

(defonce ^:private summaries (atom {}))   ;; id -> [stamp summary]
(defonce ^:private analyses (atom []))    ;; the last few full analyses: [[id stamp analysis] ...]

(defn analysis
  "The full analysis of id, the last few kept while the files do not change."
  [id]
  (let [s (stamp id)]
    (or (some (fn [[i st a]] (when (and (= i id) (= st s)) a)) @analyses)
        (let [a (analyze id)]
          (swap! analyses #(vec (take 3 (cons [id s a] (remove (comp #{id} first) %)))))
          (swap! summaries assoc id [s (summarize a)])
          a))))

(defn summary [id]
  (let [s (stamp id)
        [st sm] (get @summaries id)]
    (if (= st s) sm (summarize (analysis id)))))

(defn mirror-ids
  "The mirrors worth listing: a session that has a meta, was not skipped, is a real session's twin."
  []
  (->> ((f 'oglinda.state/all-ids))
       (keep (fn [id] (when-let [m ((f 'oglinda.state/meta-of) id)] (when-not (or (= :skipped (:state m)) (not (:cwd m))) m))))
       (map :id)))

(defn resolve-id [prefix]
  (some #(when (str/starts-with? % (str prefix)) %) (mirror-ids)))

(defn warm!
  "Read every mirror once, in the background, so the list page opens fast. Never throws; does nothing without oglinda."
  []
  (when (available?)
    (future (try (doall (pmap #(try (summary %) (catch Exception _ nil)) (mirror-ids))) (catch Throwable _ nil)))))

;; --- html: the parts -------------------------------------------------------------

(defn- page [title body & {:keys [current] :or {current :mirrors}}]
  (pg/shell title body :current current :wide true))

(defn- missing-page [e]
  (page "Mirrors" [:div [:h1 "Mirrors"] [:p.lede.bad (ex-message e)]]))

(defn- text-block
  "A long text as a pre, as tall as its text: the first limit characters, the rest behind a details (nothing on
  these pages scrolls inside a box)."
  [s limit cls]
  (let [s (str s)
        code (fn [t] [:pre {:class cls} t])]
    (if (<= (count s) limit)
      (code s)
      (let [cut (let [i (str/last-index-of s "\n" limit)] (if (and i (> i (* 0.6 limit))) i limit))]
        (list (code (subs s 0 cut))
              [:details.fold [:summary (format "the other %s characters (%s in all)" (thou (- (count s) cut)) (thou (count s)))]
               (code (subs s cut))])))))

(defn- input-view [{:keys [name input]}]
  (let [code (or (:code input) (when (= "Bash" name) (:command input)))]
    [:div.input
     (when-let [rows (seq (for [[k v] input :when (not (input-long-keys (clojure.core/name k)))] [k v]))]
       [:table.kv (for [[k v] rows] [:tr [:th (clojure.core/name k)] [:td (if (string? v) v (pr-str v))]])])
     (when code [:div [:div.tag (if (:code input) "code sent to nod" "command")] (text-block code 6000 "sent")])
     (when-let [o (:old_string input)] [:div [:div.tag.del "old"] (text-block o 3000 "old")])
     (when-let [n (:new_string input)] [:div [:div.tag.add "new"] (text-block n 3000 "new")])
     (for [k [:content :new_content] :let [v (get input k)] :when v]
       [:div [:div.tag (clojure.core/name k)] (text-block v 3000 "sent")])]))

(defn- call-view
  "One call as a details: a summary line (index, tool, what, time, state) and the body, the error first when it failed."
  [{:keys [i name nod? failed hung refused dur text label sub? pending error] :as c} & {:keys [open?]}]
  [:details {:class (str "call " (cond failed "failed" refused "refused" :else "fine") (when nod? " nod") (when sub? " sub"))
             :id (str "c" i)
             :open (when (or open? failed refused) true)}
   [:summary
    [:span.i i]
    [:span.tool (if (and nod? (not= name "mcp__nod__eval")) "nod" (str/replace name #"^mcp__nod__" "nod "))]
    (cond hung [:span.state.bad "HUNG"] failed [:span.state.bad "FAILED"] refused [:span.state.warn "refused"] pending [:span.state.warn "no answer yet"])
    [:span.lbl label]
    [:span.meta (when sub? "subagent · ") (when dur (str (short-dur dur) " · ")) (str (thou (count text)) " ch")]]
   [:div.call-body
    (when error [:div [:div.tag.err (if hung "hung" "error")] [:pre.err error]])
    (input-view c)
    [:div [:div.tag (if nod? "nod replied" "result")]
     (if (str/blank? text) [:p.muted "empty"] (text-block text 4000 (if failed "outerr" "out")))]]])

(defn- chips [m]
  (if (seq m)
    [:div.chips (for [[k n] (sort-by (juxt (comp - val) key) m)] [:span.chip (str k " " n)])]
    "–"))

(defn- side-rows
  "The rows of the two-sided table: [label hint real twin real-n twin-n], the two last numbers for the change."
  [real mirror]
  (let [g (fn [s & ks] (when s (get-in s ks)))
        n (fn [label hint fmt & ks] (let [r (apply g real ks) t (apply g mirror ks)] [label hint (when r (fmt r)) (when t (fmt t)) r t]))
        cnt (fn [label hint fmt] (let [r (when real (fmt real)) t (when mirror (fmt mirror))] [label hint (some-> r thou) (some-> t thou) r t]))]
    [(n "cost" "the tokens at list price" usd :cost)
     (n "busy time" "what the turns themselves took" dur-str :busy)
     (n "wall time" "from the prompt to its end, waits for background tasks in" dur-str :wall)
     (n "API calls" "answers of the model" thou :tokens :calls)
     (n "tokens in, new" "read for the first time" thou :tokens :input)
     (n "tokens in, from cache" "read again at a tenth of the price" thou :tokens :cache-read)
     (n "tokens written to cache" "stored for the next call" thou :tokens :cache-write)
     (n "tokens out" "what the model wrote, thinking included" thou :tokens :output)
     ["calls by tool" nil (when real (chips (:tools real))) (when mirror (chips (:tools mirror))) nil nil]
     (cnt "nod calls" nil nod-calls-of)
     (cnt "nod calls that failed" "an error from nod, or no answer" #(count (:nod-failed %)))
     (cnt "code read or edited without nod" "with Bash, Read, Edit and the like" #(count (:bypass %)))]))

(defn- side-table
  "Real and twin side by side as a table, one row per measure, with how the twin differs. hints?: a line under each
  measure saying what it is (once per page is enough)."
  [real mirror & {:keys [hints?]}]
  [:table.sides.cmp
   [:thead [:tr [:th] [:th "real session"] [:th "twin, with nod"] [:th "twin against real"]]]
   [:tbody
    (for [[label hint r t rn tn] (side-rows real mirror)]
      (let [d (when (and (number? rn) (number? tn)) (pct tn rn))]
        [:tr [:th label (when (and hints? hint) [:span.hint hint])]
         [:td (or r [:span.empty "–"])] [:td (or t [:span.empty "–"])]
         [:td.vs (pct-str d)]]))]
   (when (or (seq (:unpriced real)) (seq (:unpriced mirror)))
     [:tfoot [:tr [:td {:colspan 4 :class "muted"}
                   (str "No price for " (str/join ", " (sort (into (set (:unpriced real)) (:unpriced mirror)))) ": its tokens are not in the cost.")]]])])

(defn- guard-view
  "The guard's refusals and the # nod-cant reasons of a prompt, folded."
  [entries]
  (when (seq entries)
    (let [refused (count (filter #(= :refused (:decision %)) entries))
          hatch (count (filter #(= :hatch (:decision %)) entries))]
      [:details.fold
       [:summary (str "Guard: " refused " refused, " hatch " went around nod with a reason")]
       [:ul.plain
        (for [{:keys [decision tool kind path reason command]} entries]
          [:li
           (if (= :hatch decision) [:span.state.warn "nod-cant"] [:span.state.bad "refused"])
           " " [:b tool] (when kind (str " " (name kind))) (when path (str " " path))
           (when reason [:span.reason (str " — " reason)])
           (when command [:details.fold [:summary (clip (first-line command) 120)] (text-block command 3000 "sent")])])]])))

(defn- prompt-view
  "A prompt's text: short ones whole, long ones clipped with the whole text behind a details."
  [text]
  (let [t (str/trim (str text))]
    (if (<= (count t) 260)
      [:div.said t]
      (list [:div.said (clip t 260)]
            [:details.fold [:summary (str "Show the whole prompt (" (thou (count t)) " characters)")] (text-block t 6000 "prompt")]))))

;; --- html: the pages ------------------------------------------------------------

(defn mirrors-page [q]
  (let [ids (mirror-ids)
        rows (->> (pmap (fn [id] (try (summary id) (catch Exception e {:id id :broken (ex-message e)}))) ids)
                  (map #(assoc % :ratio (pct (:twin-cost %) (:real-cost %)))))
        sort-k (keyword (get q "sort" "created"))
        rows (case sort-k
               (:cwd :ratio) (sort-by #(or (get % sort-k) 1e9) rows)
               (sort-by #(- (or (get % sort-k) 0)) rows))
        sorter (fn [t k] [:a {:href (str "/mirrors?sort=" (name k))} t])]
    (page "Mirrors · trace"
          [:div
           [:h1 "Mirrors"]
           [:p.legend (count rows) " real Claude sessions that were run a second time, a twin: the same prompts, in a sandbox, working with nod "
            "(a tool that reads and edits code like an IDE) instead of search and replace. Costs are the tokens at list price, over the "
            "prompts both sides ran. \"twin vs real\" is how much more or less the twin cost; it does not say which side is better. "
            "Pick a session to read its prompts side by side."]
           [:table.list
            [:thead
             [:tr.groups
              [:th {:rowspan 2} (sorter "when" :created)] [:th {:rowspan 2} (sorter "project" :cwd)] [:th.n {:rowspan 2} "prompts"]
              [:th {:colspan 3} "cost"] [:th {:colspan 2} "busy time"] [:th {:colspan 2} "API calls"] [:th {:colspan 3} "nod"] [:th {:colspan 2} "guard"]]
             [:tr
              [:th.n (sorter "real" :real-cost)] [:th.n (sorter "twin" :twin-cost)] [:th.n (sorter "twin vs real" :ratio)]
              [:th.n "real"] [:th.n "twin"] [:th.n "real"] [:th.n "twin"]
              [:th.n (sorter "calls" :nod-calls)] [:th.n (sorter "failed" :nod-failed)] [:th "version"]
              [:th.n "refused"] [:th.n "nod-cant"]]]
            [:tbody
             (for [{:keys [id cwd created state prompts both real-cost twin-cost ratio real-calls twin-calls real-busy twin-busy nod-calls nod-failed
                           nod-versions refused hatch broken]} rows]
               (if broken
                 [:tr [:td.bad {:colspan 15} [:a {:href (str "/mirror/" id)} (subs id 0 8)] " could not be read: " broken]]
                 [:tr
                  [:td.when (stamp-str created)]
                  [:td [:a {:href (str "/mirror/" id)} (fs/file-name cwd)] [:span.sess (str " " (subs id 0 8))]
                   (when (not= :done state) [:span.badge (name state)])]
                  [:td.n (if (= prompts both) prompts (str both "/" prompts))]
                  [:td.n (usd real-cost)] [:td.n (usd twin-cost)] [:td.n (or (pct-str ratio) "–")]
                  [:td.n (dur-str real-busy)] [:td.n (dur-str twin-busy)]
                  [:td.n (or real-calls "–")] [:td.n (or twin-calls "–")]
                  [:td.n nod-calls] [:td.n {:class (when (pos? nod-failed) "bad")} nod-failed]
                  [:td.mono (if (seq nod-versions) (str/join ">" (map s7 nod-versions)) "–")]
                  [:td.n refused] [:td.n hatch]]))]]])))

(defn- session-notes
  "What went wrong in a session, in words, before any comparison."
  [{:keys [state reason prompts not-run real-interrupted twin-interrupted twin-errors]}]
  (let [on (fn [n] (str "on " n (if (= 1 n) " prompt." " prompts.")))]
    (cond-> []
      (= :failed state) (conj (str "The twin's run failed" (when reason (str " (" (clip reason 160) ")")) "."))
      (pos? not-run) (conj (str "The twin has not run " not-run " of " prompts (if (= 1 prompts) " prompt." " prompts.")))
      (pos? real-interrupted) (conj (str "The real session's turn was interrupted by the user " (on real-interrupted)))
      (pos? twin-interrupted) (conj (str "The twin's turn was interrupted " (on twin-interrupted)))
      (pos? twin-errors) (conj (str "The twin's turn ended with an error " (on twin-errors))))))

(defn session-sentence
  "The verdict of a whole session from its summary: what went wrong first, then the sentence over the prompts both
  sides ran, then nod's calls and failures."
  [sm]
  (let [both (:both sm)
        cmp (comparison-text {:cost (:real-cost sm) :busy (:both-real-busy sm)} {:cost (:twin-cost sm) :busy (:both-twin-busy sm)})]
    (str/join " "
              (concat (session-notes sm)
                      [(cond (and cmp (pos? both)) (str "Over the " (if (= 1 both) "prompt" (str both " prompts")) " both sides ran: "
                                                        (str/replace-first cmp #"^The twin" "the twin"))
                             (pos? both) "There is no cost or time to compare."
                             :else "There is nothing yet that both sides ran.")
                       (str "nod was called " (n-of (:nod-calls sm) "time" "times")
                            (when (pos? (:nod-calls sm)) (str "; " (thou (:nod-failed sm)) " failed")) ".")]))))

(defn- tile [label rv tv & [note]]
  [:div.tile [:span.lab label]
   [:div.two [:div [:b rv] [:span "real"]] [:div [:b tv] [:span "twin"]]]
   (when note [:span.note note])])

(defn- one-tile [label v note]
  [:div.tile [:span.lab label] [:div.two [:div [:b v] (when note [:span note])]]])

(defn- overview
  "One line per prompt: the way in to its card."
  [ps]
  [:table.list
   [:thead [:tr.groups [:th {:rowspan 2} "prompt"] [:th {:rowspan 2} "what was asked"] [:th {:colspan 3} "cost"] [:th {:colspan 2} "busy time"] [:th {:colspan 2} "nod"]]
    [:tr [:th.n "real"] [:th.n "twin"] [:th.n "twin vs real"] [:th.n "real"] [:th.n "twin"] [:th.n "calls"] [:th.n "failed"]]]
   [:tbody
    (for [{:keys [n text real mirror]} ps]
      [:tr
       [:td [:a {:href (str "#p" n)} n]] [:td (clip text 110)]
       [:td.n (usd (:cost real))] [:td.n (usd (:cost mirror))] [:td.n (or (when (and real mirror) (pct-str (pct (:cost mirror) (:cost real)))) "–")]
       [:td.n (dur-str (:busy real))] [:td.n (dur-str (:busy mirror))]
       [:td.n (if mirror (nod-calls-of mirror) "–")]
       [:td.n {:class (when (and mirror (pos? (count (:nod-failed mirror)))) "bad")} (if mirror (count (:nod-failed mirror)) "–")]])]])

(defn- prompt-card [id {:keys [n text real mirror verdict twin-calls guard nods]}]
  (let [failed (filter #(and (:nod? %) (:failed %)) twin-calls)
        other (filter #(and (not (:nod? %)) (:failed %)) twin-calls)
        refused (filter :refused twin-calls)
        shown (filter #(or (:failed %) (:refused %)) twin-calls)]
    [:section.card {:id (str "p" n)}
     [:h3 [:a {:href (str "/mirror/" id "/p/" n)} (str "Prompt " n)] (when (seq nods) [:span.mono.muted (str "nod " (str/join " > " (map s7 nods)))])]
     [:p.verdict (:text verdict)]
     (prompt-view text)
     (side-table real mirror)
     [:div.line
      (when mirror (list [:span (n-of (nod-calls-of mirror) "nod call" "nod calls")]
                         (if (seq failed) [:span.state.bad (str (count failed) " failed")] (when (pos? (nod-calls-of mirror)) [:span.state.good "none failed"]))))
      (when (seq other) [:span.state.bad (str (count other) " other tool errors")])
      (when (seq refused) [:span.state.warn (str (count refused) " refused by the guard")])
      [:a {:href (str "/mirror/" id "/p/" n)} "all calls of this prompt, both sides →"]]
     (when (seq shown)
       [:details.fold [:summary (str "The " (count shown) (if (= 1 (count shown)) " call" " calls") " of the twin that went wrong")]
        (for [c shown] (call-view c :open? true))])
     (guard-view guard)]))

(defn- all-failed?
  "Is there anything in this prompt for the only-failures view: a failed twin call or a guard entry."
  [{:keys [twin-calls guard]}]
  (or (some #(or (:failed %) (:refused %)) twin-calls) (seq guard)))

(defn- entry-view
  "One working tree of one side: where, branch, commits, files, lines."
  [repo {:keys [path main new branch commits files add del untracked counted] :as e} who]
  (if-not e
    [:tr [:th who] [:td {:colspan 5 :class "empty"} "nothing"]]
    [:tr [:th who]
     [:td (if main "main checkout" (let [p (str path) r (str repo "/")] (if (str/starts-with? p r) (subs p (count r)) p))) (when new " (new)")]
     [:td (or branch "–")]
     [:td (count commits)]
     [:td (if counted
            [:details.fold {:style "margin:0"} [:summary (str (count files) (if (= 1 (count files)) " file" " files"))]
             [:ul.plain (for [p (sort files)] [:li [:code p]])]]
            "not counted")]
     [:td (if counted (str "+" add " −" del) "–") (when (pos? (or untracked 0)) [:span.muted (str ", " untracked " untracked, not counted")])]]))

(defn- pair-text
  "What a pair of results says, in words, with the files themselves behind a details: nothing cut."
  [{r :real m :mirror :keys [identical same-files between verdict]}]
  (cond
    (not (and r m)) nil
    identical [:p.verdict "The same result."]
    same-files [:p.verdict (str "Same files, " (+ (:add between 0) (:del between 0)) " lines differ.")]
    (not (and (:counted r) (:counted m))) [:p.verdict (str (str/upper-case (subs verdict 0 1)) (subs verdict 1) ".")]
    :else
    (let [rs (set (:files r)) ms (set (:files m))
          only-r (sort (remove ms rs)) only-m (sort (remove rs ms))]
      [:div
       [:p.verdict (str "Different files: " (count only-r) " only in the real side's result, " (count only-m) " only in the twin's, "
                        (count (filter ms rs)) " in both.")]
       (when (seq only-r) [:details.fold [:summary "only in the real side's result"] [:ul.plain (for [p only-r] [:li [:code p]])]])
       (when (seq only-m) [:details.fold [:summary "only in the twin's result"] [:ul.plain (for [p only-m] [:li [:code p]])]])])))

(defn- results-section [repos]
  (list
   [:h2 "What each side left in the repos"]
   [:p.legend "oglinda takes each side's work when the side ends, so this is the whole session, not one prompt: per repo, the working trees each side made or changed, "
    "the tracked files against the state both started from (ignored files never count)."]
   (cond
     (:error repos) [:p.bad (str "The result could not be read: " (:error repos))]
     (not (some true? (vals (:recorded repos))))
     [:p.muted "Not recorded for this session: each side's work is taken when it ends, and this session's was not (still going, or from before the work was recorded)."]
     :else
     (list
      (when-not (get-in repos [:recorded :real]) [:p.muted "The real side's work was not recorded."])
      (when-not (get-in repos [:recorded :mirror]) [:p.muted "The twin's work was not recorded."])
      (if (seq (:repos repos))
        (for [{:keys [path pairs]} (:repos repos)]
          [:div.card
           [:h3 (fs/file-name path) [:span.mono.muted path]]
           (for [{r :real m :mirror :as pr} pairs]
             (list
              [:table.sides
               [:thead [:tr [:th] [:th "where"] [:th "branch"] [:th "commits"] [:th "files"] [:th "lines"]]]
               [:tbody (entry-view path r "real session") (entry-view path m "twin, with nod")]]
              (pair-text pr)))])
        [:p.muted "No change on either side."])))))

(defn- nod-section
  "nod's own calls over the session, its hung and slow calls, and every failure with its error."
  [id ps]
  (let [twin (sum-of ps :mirror)
        failed (for [{:keys [n twin-calls]} ps c twin-calls :when (and (:nod? c) (:failed c))] [n c])
        hung (for [{:keys [n mirror]} ps h (:nod-hung mirror)] [n h])]
    (list
     [:h2 "nod's own calls and its failures"]
     (if (zero? (nod-calls-of twin))
       [:p.muted "The twin made no nod call."]
       (list
        [:p (n-of (nod-calls-of twin) "nod call" "nod calls") " by the twin, by what they asked nod to do:"]
        (chips (:nod twin))
        [:p.muted "The twin waited " [:b (dur-str (:nod-wait twin))] " for nod in all"
         (when (seq (:nod-slow twin)) (str "; " (n-of (count (:nod-slow twin)) "call took" "calls took") " over a minute"))
         (when (seq (:nod-hung twin)) (str "; " (count (:nod-hung twin)) " got no answer at all"))
         "."]))
     (if (seq failed)
       (list
        [:p [:b (count failed)] (if (= 1 (count failed)) " nod call failed" " nod calls failed") " (nod's own error, or no answer). The link opens the call."]
        [:table.list
         [:thead [:tr [:th "prompt"] [:th.n "call"] [:th "what the twin asked"] [:th "what nod said"]]]
         [:tbody
          (for [[n c] failed]
            [:tr [:td [:a {:href (str "/mirror/" id "/p/" n "#c" (:i c))} n]] [:td.n (:i c)]
             [:td [:code (clip (:label c) 120)]] [:td (clip (:error c) 200)]])]])
       (when (pos? (nod-calls-of twin)) [:p.ok "No nod call failed."]))
     (when (seq hung)
       [:p.bad (str (count hung) " nod " (if (= 1 (count hung)) "call" "calls") " got no answer: ")
        (str/join "; " (for [[n h] hung] (str "prompt " n ": " (clip (:cmd h) 80))))]))))

(defn session-page [id q]
  (let [a (analysis id)
        sm (summarize a)
        only? (= "failures" (get q "only"))
        ps (cond->> (:prompts a) only? (filter all-failed?))
        real (sum-of (:prompts a) :real)
        twin (sum-of (:prompts a) :mirror)
        any-real? (some :real (:prompts a)) any-twin? (some :mirror (:prompts a))
        pc (pct (:twin-cost sm) (:real-cost sm))]
    (page (str (fs/file-name (:cwd a)) " · mirror")
          [:div
           [:p.crumb [:a {:href "/mirrors"} "Mirrors"] " / " (fs/file-name (:cwd a))]
           [:h1 (fs/file-name (:cwd a)) " " [:span.sess (str (subs id 0 8) " · " (stamp-str (:created a)) " · " (name (:state a)))]]
           [:p.lede (session-sentence sm)]
           [:p.muted "First prompt: " (clip (:first-prompt a) 220)]
           [:div.tiles
            (tile "cost" (usd (:real-cost sm)) (usd (:twin-cost sm)) (when pc (str "twin " (pct-str pc) " against real")))
            (tile "busy time" (dur-str (:real-busy sm)) (dur-str (:twin-busy sm)))
            (tile "API calls" (thou (:real-calls sm)) (thou (:twin-calls sm)))
            (tile "tokens out" (thou (get-in real [:tokens :output])) (thou (get-in twin [:tokens :output])))
            (one-tile "nod" (n-of (:nod-calls sm) "call" "calls")
                      (str (thou (:nod-failed sm)) " failed" (when (seq (:nod-versions sm)) (str " · nod " (str/join ">" (map s7 (:nod-versions sm)))))))
            (one-tile "guard" (str (:refused sm) " refused") (str (:hatch sm) " went around nod with a reason"))]
           [:h2 "Totals"]
           [:p.legend (str "Everything each side did in the " (count (:prompts a)) (if (= 1 (count (:prompts a))) " prompt" " prompts") " of the session"
                           (when (not= (:both sm) (:prompts sm)) (str " (the cost in the sentence above counts only the " (:both sm) " both ran)")) ".")]
           (side-table (when any-real? real) (when any-twin? twin) :hints? true)
           [:h2 "Prompt by prompt"]
           (overview (:prompts a))
           [:div.filters
            (if only?
              (list [:a {:href (str "/mirror/" id)} "All prompts"] [:b "Only prompts with a failed call or a guard entry"])
              (list [:b "All prompts"] [:a {:href (str "/mirror/" id "?only=failures")} "Only prompts with a failed call or a guard entry"]))]
           (if (seq ps)
             (for [p ps] (prompt-card id p))
             [:p.muted "No failed twin call and no guard entry in this session."])
           (results-section (:repos a))
           (nod-section id (:prompts a))])))

(defn- col-calls [title calls show]
  (let [calls (case show
                "nod" (filter :nod? calls)
                "failed" (filter :failed calls)
                calls)
        nods (count (filter :nod? calls))]
    [:section
     [:h2 title " " [:span.muted {:style "font-weight:400;font-size:.9rem"}
                     (str (n-of (count calls) "call" "calls") (when (pos? nods) (str ", " nods " nod")) (when-let [n (seq (filter :failed calls))] (str ", " (count n) " failed")))]]
     (if (seq calls)
       [:div.calls (for [c calls] (call-view c :open? (:nod? c)))]
       [:p.muted "No calls" (when (not= show "all") " of this kind") "."])]))

(defn prompt-page [id n q]
  (let [a (analysis id)
        p (first (filter #(= n (:n %)) (:prompts a)))
        show (get q "show" "all")]
    (if-not p
      (page "Mirror" [:p.muted "No such prompt."])
      (page (str "prompt " n " · mirror")
            [:div
             [:p.crumb [:a {:href "/mirrors"} "Mirrors"] " / " [:a {:href (str "/mirror/" id)} (str (fs/file-name (:cwd a)) " " (subs id 0 8))] " / prompt " n]
             [:h1 (str "Prompt " n)]
             [:p.lede (:text (:verdict p))]
             (prompt-view (:text p))
             (side-table (:real p) (:mirror p))
             (guard-view (:guard p))
             [:div.filters "Show "
              (for [[k t] [["all" "all calls"] ["nod" "nod calls only"] ["failed" "failures only"]]]
                (if (= k show) [:b t] [:a {:href (str "/mirror/" id "/p/" n "?show=" k)} t]))]
             [:div.cols2
              (col-calls "Real session" (:real-calls p) show)
              (col-calls "Twin, with nod" (:twin-calls p) show)]]))))

;; --- routing ---------------------------------------------------------------------

(defn route
  "The response for a /mirrors or /mirror/... uri (q: its query params as a map), or nil when it is not ours."
  [uri q]
  (let [html (fn [s] {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body s})]
    (try
      (cond
        (= uri "/mirrors") (html (mirrors-page q))
        (re-matches #"/mirror/[^/]+" uri)
        (if-let [id (resolve-id (subs uri 8))] (html (session-page id q)) {:status 404 :body "no such mirror"})
        (re-matches #"/mirror/[^/]+/p/\d+" uri)
        (let [[_ pre n] (re-matches #"/mirror/([^/]+)/p/(\d+)" uri)]
          (if-let [id (resolve-id pre)] (html (prompt-page id (parse-long n) q)) {:status 404 :body "no such mirror"})))
      (catch clojure.lang.ExceptionInfo e
        (if (missing? e) (html (missing-page e)) (throw e))))))
