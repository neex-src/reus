(ns trace.mirror
  "The viewer's pages for the nod mirrors of oglinda (~/projects/oglinda): a real Claude session next to its
  twin, prompt by prompt, and every call of both sides with its input and what came back.

    /mirrors                        every mirrored session: cost, API calls, time, nod calls and failures
    /mirror/<id>[?only=failures]    one session: per prompt both sides side by side and a verdict
    /mirror/<id>/p/<n>[?show=nod|failed]   one prompt: the calls of the real side and of the twin, expandable

  oglinda is found at run time, like trace.ash finds ashamer, and is NOT in bb.edn :deps: bb resolves those
  for every task, the hook's too, and the recording path (hook, worker, record) must never need oglinda.
  OGLINDA_HOME says where it is, else ~/projects/oglinda. Absent, only these pages say so; nothing else notices.

  The numbers are oglinda.compare's own (side, mirror-turns, real-turns, with-cost: one definition of busy,
  wall, cost, nod call and failure for both sides). What this namespace adds is the per-call detail
  `compare` counts and throws away: the code sent, nod's reply, how long it took."
  (:require [babashka.classpath :as cp]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [hiccup2.core :as h])
  (:import [java.net URLEncoder]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

;; --- oglinda, found at run time ------------------------------------------------

(defn home [] (or (System/getenv "OGLINDA_HOME") (str (fs/path (System/getProperty "user.home") "projects" "oglinda"))))

(defn available? [] (fs/exists? (fs/path (home) "src" "oglinda" "compare.clj")))

(defonce ^:private loaded (atom false))

(defn load!
  "Put oglinda on the classpath (once; its src and its resources, where config.edn is); throws ex-info
  {:oglinda :missing} when it is not there."
  []
  (when-not @loaded
    (when-not (available?)
      (throw (ex-info (str "oglinda is not installed (looked in " (home) "; OGLINDA_HOME says where): the mirror pages need it")
                      {:oglinda :missing})))
    (cp/add-classpath (str (fs/path (home) "src")))
    (cp/add-classpath (str (fs/path (home) "resources")))
    (require 'oglinda.compare 'oglinda.state 'oglinda.config 'oglinda.transcript 'oglinda.repin 'oglinda.around)
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
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- call-label [{:keys [name input nod?]}]
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

;; --- one session, analysed ------------------------------------------------------

(defn- ms-of [ts] (try (.toEpochMilli (Instant/parse ts)) (catch Exception _ nil)))

(defn- turn-start [ls] (some->> ls (keep #(ms-of (:timestamp %))) seq (apply min)))

(defn- verdict
  "A plain verdict on a prompt from its two sides: {:class :text}. Cost leads; the time is said beside it."
  [real mirror]
  (cond
    (not mirror) {:class "none" :text "the twin has not run this prompt"}
    (not real) {:class "none" :text "the real session has no transcript for this prompt"}
    (or (nil? (:cost real)) (zero? (:cost real))) {:class "none" :text "no cost to compare"}
    :else
    (let [pct (fn [m r] (when (and m r (pos? r)) (* 100.0 (/ (- m r) r))))
          c (pct (:cost mirror) (:cost real))
          t (pct (:busy mirror) (:busy real))
          word (fn [p cheap dear] (cond (< (Math/abs p) 5) "about the same" (neg? p) (format "%s by %.0f%%" cheap (- p)) :else (format "%s by %.0f%%" dear p)))]
      {:class (cond (< (Math/abs c) 5) "same" (neg? c) "cheaper" :else "dearer")
       :text (str "twin " (word c "cheaper" "dearer") " (" (format "$%.2f vs $%.2f" (:cost mirror) (:cost real)) ")"
                  (when t (str ", " (word t "faster" "slower") " busy")))})))

(defn- queue-entries [id]
  (let [st-queued (f 'oglinda.state/queued) st-mfile (f 'oglinda.state/mfile) read-edn (f 'oglinda.config/read-edn)]
    (into (sorted-map) (for [n (st-queued id)] [n (read-edn (st-mfile id "queue" (format "%04d.edn" n)))]))))

(defn analyze
  "Everything the pages show of mirror id: the session's header and per prompt both sides (oglinda.compare's
  `side` plus cost), the verdict, the call lists of both, the guard's entries inside the prompt."
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
                                 mirror (when mx (with-cost prices (side (:lines mx) mx exts)))]]
                       {:n n :text (str/join " " (keep :text (:content e)))
                        :nods (get turn-nods n)
                        :real real :mirror mirror
                        :verdict (verdict real mirror)
                        :real-calls (when r (call-list (:lines r)))
                        :twin-calls (when mx (call-list (:lines mx)))
                        :guard (vec (sort-by :at (get guard-by n)))}))]
    {:id id :cwd (:cwd mm) :created (:created mm) :state (:state mm) :first-prompt (:first-prompt mm)
     :nod (get-in mm [:tools :nod :sha]) :repins (:repins mm) :guard guard
     :prompts prompts}))

(defn- sum-of [ps k]
  ((f 'oglinda.compare/sum-sides) ps k))

(defn summarize
  "A session for the list: the sums over its prompts, without the call lists."
  [{:keys [prompts] :as a}]
  (let [ps (map #(dissoc % :real-calls :twin-calls) prompts)
        real (sum-of ps :real) mirror (sum-of ps :mirror)
        calls #(+ (get-in % [:tools "nod-eval"] 0) (get-in % [:tools "nod"] 0))
        both (filter #(and (:real %) (:mirror %)) ps)
        sum-cost (fn [k] (reduce + 0.0 (keep #(get-in % [k :cost]) both)))]
    (-> (select-keys a [:id :cwd :created :state :nod :repins])
        (assoc :prompts (count prompts)
               :both (count both)
               ;; cost over the prompts both sides ran, so a half-run twin is not "cheaper"
               :real-cost (sum-cost :real) :twin-cost (sum-cost :mirror)
               :real-calls (get-in real [:tokens :calls]) :twin-calls (get-in mirror [:tokens :calls])
               :real-busy (:busy real) :twin-busy (:busy mirror)
               :nod-calls (calls mirror) :nod-failed (count (:nod-failed mirror))
               :nod-versions (vec (distinct (mapcat :nods prompts)))
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

;; --- html -----------------------------------------------------------------------

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))
(defn- stamp-str [ms] (when ms (.format (DateTimeFormatter/ofPattern "MMM d, HH:mm") (.atZone (Instant/ofEpochMilli ms) (ZoneId/systemDefault)))))
(defn- usd [x] (if x (format "$%.2f" (double x)) "-"))
(defn- dur-str [s]
  (when s
    (let [s (Math/round (double s))]
      (cond (>= s 3600) (format "%dh %02dm" (quot s 3600) (quot (mod s 3600) 60))
            (>= s 60) (format "%dm %02ds" (quot s 60) (mod s 60))
            :else (str s "s")))))
(defn- short-dur [s] (when s (if (< s 10) (format "%.1fs" (double s)) (dur-str s))))
(defn- s7 [x] (some-> x str (subs 0 (min 7 (count (str x))))))

(defn- pct [m r] (when (and m r (pos? r)) (* 100.0 (/ (- m r) r))))
(defn- pct-str [p] (when p (format "%+.0f%%" p)))

(defn- page [title body]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title]
          [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
          [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;600&display=swap"}]
          [:link {:rel "stylesheet" :href "/static/app.css"}]
          [:link {:rel "stylesheet" :href "/static/mirror.css"}]
          [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]
          [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/clojure.min.js"}]]
         [:body
          [:header.top
           [:a.brand {:href "/"} "reus trace"]
           [:nav [:a {:href "/"} "Prompts"] [:a.on {:href "/mirrors"} "Mirrors"]]]
          body
          [:script (h/raw "try{document.querySelectorAll('code[data-hl]').forEach(function(e){hljs.highlightElement(e)})}catch(e){}")]]])))

(defn- missing-page [e]
  (page "Mirrors" [:main.wide [:h1 "Mirrors"] [:p.note.mx-missing (ex-message e)]]))

(defn- text-block
  "A long text as a pre: the first limit characters, the rest behind a details (the page itself never
  scrolls inside a box)."
  [s limit cls & [lang]]
  (let [s (str s)
        code (fn [t] [:pre {:class (str "code " cls)} [:code (cond-> {} lang (assoc :class (str "language-" lang) :data-hl "1")) t]])]
    (if (<= (count s) limit)
      (code s)
      (let [cut (let [i (str/last-index-of s "\n" limit)] (if (and i (> i (* 0.6 limit))) i limit))]
        (list (code (subs s 0 cut))
              [:details.rest [:summary (format "the other %d characters (%d in all)" (- (count s) cut) (count s))]
               (code (subs s cut))])))))

(defn- input-view [{:keys [name input nod?]}]
  (let [code (or (:code input) (when (= "Bash" name) (:command input)))
        lang (cond (:code input) "clojure" (= "Bash" name) "bash")]
    [:div.input
     (when-let [rows (seq (for [[k v] input :when (not (input-long-keys (clojure.core/name k)))] [k v]))]
       [:table.kv (for [[k v] rows] [:tr [:th (clojure.core/name k)] [:td (if (string? v) v (pr-str v))]])])
     (when code [:div.pair [:div.tag (if (:code input) "code sent to nod" "command")] (text-block code 6000 "sent" lang)])
     (when-let [o (:old_string input)] [:div.pair [:div.tag.del "old"] (text-block o 3000 "old")])
     (when-let [n (:new_string input)] [:div.pair [:div.tag.add "new"] (text-block n 3000 "new")])
     (for [k [:content :new_content] :let [v (get input k)] :when v]
       [:div.pair [:div.tag (clojure.core/name k)] (text-block v 3000 "sent")])]))

(defn- call-view
  "One call as a details: a summary line (index, tool, what, time, state) and the body, the error first when it failed."
  [{:keys [i name nod? failed hung refused dur text label sub? pending error] :as c} & {:keys [open?]}]
  [:details {:class (str "call " (cond failed "failed" refused "refused" :else "ok") (when nod? " nod") (when sub? " sub"))
             :id (str "c" i)
             :open (when (or open? failed refused) true)}
   [:summary
    [:span.i i]
    [:span.tool (if (and nod? (not= name "mcp__nod__eval")) "nod" (str/replace name #"^mcp__nod__" "nod "))]
    (cond hung [:span.state.bad "HUNG"] failed [:span.state.bad "FAILED"] refused [:span.state.warn "refused"] pending [:span.state.warn "no answer yet"])
    [:span.lbl label]
    [:span.meta (when sub? [:span.subtag "subagent"]) (when dur [:span.t (short-dur dur)]) [:span.size (str (count text) " ch")]]]
   [:div.call-body
    (when error [:div.errbox [:div.tag.err (if hung "hung" "error")] [:pre.code.err [:code error]]])
    (input-view c)
    [:div.pair [:div.tag (if nod? "nod replied" "result")]
     (if (str/blank? text) [:div.note "empty"] (text-block text 4000 (if failed "outerr" "out")))]]])

(defn- side-table
  "Real and twin side by side as a table, one row per measure."
  [real mirror]
  (let [row (fn [label a b & [cls]]
              [:tr {:class cls} [:th label] [:td a] [:td b]])
        tl (fn [s] (when s (let [t (:tools s)] (str/join ", " (for [[k n] (sort-by (comp - val) t)] (str k " " n))))))]
    [:table.sides
     [:thead [:tr [:th] [:th "real"] [:th "twin"]]]
     [:tbody
      (row "cost" (usd (:cost real)) (usd (:cost mirror)))
      (row "API calls" (get-in real [:tokens :calls]) (get-in mirror [:tokens :calls]))
      (row "busy / wall" (when real (str (dur-str (:busy real)) " / " (dur-str (:wall real)))) (when mirror (str (dur-str (:busy mirror)) " / " (dur-str (:wall mirror)))))
      (row "output tokens" (get-in real [:tokens :output]) (get-in mirror [:tokens :output]))
      (row "tools" (tl real) (tl mirror) "toolrow")]]))

(defn- verdict-view [{:keys [class text]}]
  [:div {:class (str "verdict v-" class)} text])

(defn- guard-view
  "The guard's refusals and the # nod-cant reasons of a prompt."
  [entries]
  (when (seq entries)
    [:div.guard
     [:div.reply-h (str "Guard · " (count (filter #(= :refused (:decision %)) entries)) " refused, "
                        (count (filter #(= :hatch (:decision %)) entries)) " went around nod with a reason")]
     [:ul
      (for [{:keys [decision tool kind path reason command at]} entries]
        [:li {:class (name decision)}
         (if (= :hatch decision)
           [:span.state.warn "nod-cant"] [:span.state.bad "refused"])
         " " [:b tool] (when kind (str " " (name kind))) (when path (str " " path))
         (when reason [:span.reason (str " — " reason)])
         (when command [:details [:summary (clip (first-line command) 120)] (text-block command 3000 "sent" "bash")])])]]))

;; --- pages -----------------------------------------------------------------------

(def ^:private list-cols
  [["when" :created :desc] ["project" :cwd :asc] ["real $" :real-cost :desc] ["twin $" :twin-cost :desc]
   ["twin vs real" :ratio :asc] ["nod calls" :nod-calls :desc] ["failed" :nod-failed :desc]])

(defn mirrors-page [q]
  (let [ids (mirror-ids)
        rows (->> (pmap (fn [id] (try (summary id) (catch Exception e {:id id :broken (ex-message e)}))) ids)
                  (map #(assoc % :ratio (pct (:twin-cost %) (:real-cost %)))))
        sort-k (keyword (get q "sort" "created"))
        rows (case sort-k
               (:cwd :ratio) (sort-by #(or (get % sort-k) 1e9) rows)
               (sort-by #(- (or (get % sort-k) 0)) rows))]
    (page "Mirrors · trace"
          [:main.wide
           [:h1 "Mirrors"]
           [:p.sub (count rows) " real sessions with a nod twin. Costs are the tokens at list price, over the prompts both sides ran. "
            "Pick a session to see its prompts side by side."]
           [:table.mirrors
            [:thead [:tr
                     (for [[t k] (map (juxt first second) list-cols)] [:th [:a {:href (str "/mirrors?sort=" (name k))} t]])
                     [:th "prompts"] [:th "API calls real / twin"] [:th "busy real / twin"] [:th "guard"] [:th "nod"]]]
            [:tbody
             (for [{:keys [id cwd created state prompts both real-cost twin-cost ratio real-calls twin-calls real-busy twin-busy nod-calls nod-failed
                           nod-versions refused hatch broken]} rows]
               (if broken
                 [:tr.bad [:td {:colspan 12} [:a {:href (str "/mirror/" id)} (subs id 0 8)] " could not be read: " broken]]
                 [:tr
                  [:td (stamp-str created)]
                  [:td [:a {:href (str "/mirror/" id)} (fs/file-name cwd)] [:span.sess (str " " (subs id 0 8))]
                   (when (not= :done state) [:span.badge (name state)])]
                  [:td.num (usd real-cost)]
                  [:td.num (usd twin-cost)]
                  [:td.num {:class (cond (nil? ratio) "" (< ratio -5) "good" (> ratio 5) "bad")} (or (pct-str ratio) "-")]
                  [:td.num nod-calls]
                  [:td.num {:class (when (pos? nod-failed) "bad")} nod-failed]
                  [:td.num (if (= prompts both) prompts (str both "/" prompts))]
                  [:td.num (or real-calls "-") " / " (or twin-calls "-")]
                  [:td.num (dur-str real-busy) " / " (dur-str twin-busy)]
                  [:td.num (when (pos? refused) (str refused " refused")) (when (and (pos? refused) (pos? hatch)) ", ") (when (pos? hatch) (str hatch " nod-cant"))]
                  [:td.mono (if (seq nod-versions) (str/join ">" (map s7 nod-versions)) "-")]]))]]])))

(defn- prompt-card [id {:keys [n text real mirror verdict twin-calls guard nods]} only-failures?]
  (let [failed (filter #(and (:nod? %) (:failed %)) twin-calls)
        other (filter #(and (not (:nod? %)) (:failed %)) twin-calls)
        refused (filter :refused twin-calls)
        shown (filter #(or (:failed %) (:refused %)) twin-calls)
        nod-calls (count (filter :nod? twin-calls))]
    [:section.pcard {:id (str "p" n)}
     [:div.pcard-h
      [:a.pno {:href (str "/mirror/" id "/p/" n)} (str "prompt " n)]
      (verdict-view verdict)
      (when (seq nods) [:span.mono.nodv (str/join ">" (map s7 nods))])]
     [:details.ptext {:open (when (<= (count text) 240) true)}
      [:summary (clip text 200)]
      [:div.md.prompt-text (text-block text 5000 "prompt")]]
     [:div.pcard-b
      (side-table real mirror)
      [:div.nodline
       [:b nod-calls] " nod calls"
       (if (seq failed) [:span.state.bad (str (count failed) " failed")] (when (pos? nod-calls) [:span.state.good "none failed"]))
       (when (seq other) [:span.state.bad (str (count other) " other tool errors")])
       (when (seq refused) [:span.state.warn (str (count refused) " refused by the guard")])
       (when (seq guard) [:span.state.warn (str (count guard) " guard entries")])
       [:a.more {:href (str "/mirror/" id "/p/" n)} "all calls of this prompt →"]]]
     (when (seq shown)
       [:div.failed-calls (for [c shown] (call-view c :open? true))])
     (when (seq guard) (guard-view guard))]))

(defn- all-failed?
  "Is there anything in this prompt for the only-failures view: a failed twin call or a guard entry."
  [{:keys [twin-calls guard]}]
  (or (some #(or (:failed %) (:refused %)) twin-calls) (seq guard)))

(defn session-page [id q]
  (let [a (analysis id)
        sm (summarize a)
        only? (= "failures" (get q "only"))
        ps (cond->> (:prompts a) only? (filter all-failed?))]
    (page (str (fs/file-name (:cwd a)) " · mirror")
          [:main.wide
           [:p.crumb [:a {:href "/mirrors"} "Mirrors"] " / " (fs/file-name (:cwd a))]
           [:h1 (fs/file-name (:cwd a)) [:small.sess (str " " (subs id 0 8) " · " (stamp-str (:created a)) " · " (name (:state a)))]]
           [:p.sub (clip (:first-prompt a) 200)]
           [:div.totals
            [:div.tot [:span "cost"] [:b (usd (:real-cost sm))] " real · " [:b (usd (:twin-cost sm))] " twin "
             (when-let [p (pct (:twin-cost sm) (:real-cost sm))] [:span {:class (str "pill " (if (< p -5) "good" (if (> p 5) "bad" "")))} (pct-str p)])]
            [:div.tot [:span "API calls"] [:b (:real-calls sm)] " real · " [:b (:twin-calls sm)] " twin"]
            [:div.tot [:span "busy"] [:b (dur-str (:real-busy sm))] " real · " [:b (dur-str (:twin-busy sm))] " twin"]
            [:div.tot [:span "nod"] [:b (:nod-calls sm)] " calls · "
             [:b {:class (when (pos? (:nod-failed sm)) "bad")} (:nod-failed sm)] " failed"
             (when (seq (:nod-versions sm)) [:span.mono (str " · nod " (str/join ">" (map s7 (:nod-versions sm))))])]
            [:div.tot [:span "guard"] [:b (:refused sm)] " refused · " [:b (:hatch sm)] " nod-cant"]]
           [:div.filters
            (if only?
              (list [:a {:href (str "/mirror/" id)} "All prompts"] [:b "Only failures and guard entries"])
              (list [:b "All prompts"] [:a {:href (str "/mirror/" id "?only=failures")} "Only failures and guard entries"]))]
           (if (seq ps)
             (for [p ps] (prompt-card id p only?))
             [:p.note "No failed nod call and no guard entry in this session."])])))

(defn- col-calls [title calls show]
  (let [calls (case show
                "nod" (filter :nod? calls)
                "failed" (filter :failed calls)
                calls)
        nods (count (filter :nod? calls))]
    [:section.col
     [:h2 title [:small (count calls) " calls" (when (pos? nods) (str ", " nods " nod")) (when-let [n (seq (filter :failed calls))] (str ", " (count n) " failed"))]]
     (if (seq calls)
       [:div.calls (for [c calls] (call-view c :open? (:nod? c)))]
       [:p.note "No calls" (when (not= show "all") " of this kind") "."])]))

(defn prompt-page [id n q]
  (let [a (analysis id)
        p (first (filter #(= n (:n %)) (:prompts a)))
        show (get q "show" "all")]
    (if-not p
      (page "Mirror" [:main.wide [:p.note "No such prompt."]])
      (page (str "prompt " n " · mirror")
            [:main.wide
             [:p.crumb [:a {:href "/mirrors"} "Mirrors"] " / " [:a {:href (str "/mirror/" id)} (str (fs/file-name (:cwd a)) " " (subs id 0 8))] " / prompt " n]
             [:div.pcard-h [:h1 (str "Prompt " n)] (verdict-view (:verdict p))]
             [:details.ptext {:open true} [:summary "The prompt"] [:div.prompt-text (text-block (:text p) 1500 "prompt")]]
             (side-table (:real p) (:mirror p))
             (guard-view (:guard p))
             [:div.filters "Show "
              (for [[k t] [["all" "all calls"] ["nod" "nod calls only"] ["failed" "failures only"]]]
                (if (= k show) [:b t] [:a {:href (str "/mirror/" id "/p/" n "?show=" k)} t]))]
             [:div.cols2
              (col-calls "Real session" (:real-calls p) show)
              (col-calls "Twin (nod)" (:twin-calls p) show)]]))))

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
