(ns trace.dash
  "bb gui: the one way in. A local server (127.0.0.1, default :7350) with a dashboard, and the browser opened on it.

    bb gui [--port N] [--no-open]

  On start it only LOOKS (trace.found): Claude Code's transcripts, Copilot's sessions (the CLI and the IntelliJ
  plugin), the reus hooks, the trace repos of this checkout, the worker, health. Nothing changes until a button
  is pressed, and a button is the only thing that writes:
    build from old logs   trace.replay (Claude, the projects picked) into var/replay, trace.copilot-replay (the
                          sessions picked) into var/copilot-replay: read only towards ~/.claude, ~/.copilot and the
                          project repos, no hook turned on. Built in <repo>.building, swapped in when whole (the old
                          one stays until then); one build at a time, its progress polled by the page (/x/job).
    hooks on / off        trace.install (the Claude settings.json) and trace.copilot-install (Copilot's
                          hooks/reus.json and skill): the only things that write outside reus, their exact text
                          shown on the page before the click.
  A POST needs the token of this run (in the page; sent as a header, which no other web page can send here) and a
  127.0.0.1/localhost Host.

  Browsing: /browse?r=KEY[&m=PROJECT][&p=PROMPT-ID] starts the viewer (trace.gui/serve!) of that repo in this
  process on the next free port and sends the browser there; the viewer's top bar links back. KEY: live,
  claude-replay, copilot-replay, or the dir name of another repo in var/. bb trace:gui stays as it was: one
  repo, no dashboard.

  Routes: /  /browse  /x/ping  /x/job  POST /x/build {source claude|copilot, keys [...]}  POST /x/hooks {which
  claude|copilot, to on|off}  /static/dash.css  /static/dash.js"
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as srv]
            [trace.copilot-install :as ci]
            [trace.copilot-replay :as cr]
            [trace.found :as found]
            [trace.git :as g]
            [trace.gui :as gui]
            [trace.install :as install]
            [trace.replay :as rp])
  (:import [java.net URLDecoder URLEncoder]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(def default-port 7350)

(defonce ^:private state (atom {:token nil :port nil :job nil :viewers {}}))

;; --- small formatters ---------------------------------------------------------------------------

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))

(defn- tilde [p]
  (let [h (str (fs/home)) p (str p)]
    (if (and (not (str/blank? h)) (str/starts-with? p (str h "/"))) (str "~" (subs p (count h))) p)))

(defn- size [n]
  (let [n (or n 0)]
    (cond (>= n 1e9) (format "%.1f GB" (/ n 1e9))
          (>= n 1e6) (format "%.0f MB" (/ n 1e6))
          (>= n 1e3) (format "%.0f KB" (/ n 1e3))
          :else (str n " B"))))

(defn- ago
  "ms since epoch as `3 min ago`, `yesterday 14:02`, `Nov 3`."
  [ms]
  (when ms
    (let [d (quot (- (System/currentTimeMillis) ms) 1000)
          z (.atZone (Instant/ofEpochMilli ms) (ZoneId/systemDefault))
          f #(.format (DateTimeFormatter/ofPattern %) z)]
      (cond (< d 90) "just now"
            (< d 3600) (str (quot d 60) " min ago")
            (< d 86400) (str (quot d 3600) " h ago")
            (< d 172800) (str "yesterday " (f "HH:mm"))
            (< d (* 300 86400)) (f "MMM d")
            :else (f "MMM d, yyyy")))))

(defn- plural [n one many] (str n " " (if (= 1 n) one many)))
(defn- short-id [s] (let [s (str s)] (subs s 0 (min 8 (count s)))))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))

;; --- the viewers --------------------------------------------------------------------------------

(defn- free-port [from]
  (let [taken (set (map :port (vals (:viewers @state))))]
    (first (filter #(and (not (taken %)) (not= % (:port @state)) (not (found/port-open? %))) (range from (+ from 200))))))

(defn- dash-url [] (str "http://127.0.0.1:" (:port @state) "/"))

(defn viewer!
  "The port of the viewer of trace repo dir, started now when it is not running yet."
  [dir]
  (locking state
    (or (get-in @state [:viewers dir :port])
        (let [port (free-port (inc (or (:port @state) default-port)))
              stop (gui/serve! dir port :dash (dash-url))]
          (swap! state assoc-in [:viewers dir] {:port port :stop stop})
          port))))

(defn- repo-key [k d] (if (keyword? k) (name k) (str (fs/file-name d))))

(defn- repo-dir
  "The dir of the trace repo a /browse key names, nil for one this checkout does not know."
  [key]
  (some (fn [[k d]] (when (= key (repo-key k d)) d)) (found/repo-dirs)))

;; --- jobs: building trace repos from old logs ---------------------------------------------------

(defn job [] (:job @state))
(defn- job! [f & args] (swap! state #(assoc % :job (apply f (:job %) args))))

(defn- target [source] (get found/*replays* (case source :claude :claude-replay :copilot :copilot-replay)))

(defn- summary
  "What a finished build says, in words."
  [source stats]
  (let [{:keys [prompt tool subagent-start post]} stats]
    (str (plural (or prompt 0) "prompt" "prompts") ", " (plural (or tool 0) "tool call" "tool calls")
         (when (pos? (or subagent-start 0)) (str ", " (plural subagent-start "agent run" "agent runs")))
         " recorded"
         (when (and (= source :copilot) post)
           (str "; files checked against Copilot's own hash after each prompt: " (or (:ok post) 0) " right, "
                (or (:bad post) 0) " wrong")))))

(defn- files-of
  "The logs to replay for the keys picked (project dirs for Claude, session ids for Copilot): only ones the
  machine has."
  [source keys]
  (case source
    :claude (found/claude-files keys)
    :copilot (let [want (set keys)] (vec (keep #(when (want (:id %)) (:log %)) (:sessions (found/copilot)))))))

(defn build!
  "Build the trace repo of source (:claude or :copilot) from the logs of keys, in this thread; the job in
  state says how far it is. The repo is made in <target>.building and moved over the old one when whole."
  [source keys]
  (let [repo (target source)
        tmp (str repo ".building")
        files (files-of source keys)
        t0 (System/currentTimeMillis)]
    (job! (constantly {:id t0 :source source :state "reading" :repo repo :files (count files) :read 0
                       :events 0 :done 0 :started t0}))
    (try
      (when (empty? files) (throw (ex-info "nothing picked to build from" {})))
      (when (some #{(str (fs/absolutize repo))} (map #(str (fs/absolutize %)) [g/default-repo g/*repo*]))
        (throw (ex-info (str "refusing to build into the live trace repo " repo) {})))
      (fs/delete-tree tmp)
      (let [opts {:on-file (fn [_] (job! update :read inc))
                  :progress (fn [i n] (job! assoc :state "recording" :done i :events n))}
            stats (case source
                    :claude (binding [rp/*history-dir* (str (fs/path found/*claude-dir* "file-history"))]
                              (rp/replay! files tmp {} opts))
                    :copilot (cr/replay! files tmp {} opts))]
        (job! assoc :state "swapping")
        (when (fs/exists? repo) (fs/delete-tree repo))
        (fs/move tmp repo)
        (job! assoc :state "done" :ended (System/currentTimeMillis) :summary (summary source stats)
              :stats (select-keys stats [:prompt :tool :stop :subagent-start :post])))
      (catch Throwable e
        (binding [*out* *err*] (println "build" (name source) "failed:" (ex-message e)))
        (job! assoc :state "failed" :ended (System/currentTimeMillis) :error (or (ex-message e) (str e))))))
  (job))

(defn- running? [] (#{"reading" "recording" "swapping"} (:state (job))))

(defn- start-build!
  "Start build! in the background unless one is running; the job, or nil when one was running."
  [source keys]
  (locking state
    (when-not (running?)
      (job! (constantly {:source source :state "reading" :files 0 :read 0 :started (System/currentTimeMillis)}))
      (future (build! source keys))
      (job))))

;; --- hooks --------------------------------------------------------------------------------------

(defn- bb-edn [] (str (fs/path g/home "bb.edn")))

(defn switch-hooks!
  "Turn the reus hooks of which (:claude or :copilot) on or off (to); what the install printed."
  [which to]
  (with-out-str
    (case [which to]
      [:claude :on] (install/on (bb-edn))
      [:claude :off] (install/off)
      [:copilot :on] (ci/install! found/*copilot-home*)
      [:copilot :off] (ci/uninstall! found/*copilot-home*))))

(defn- hook-lines
  "The hook entries on would add to the Claude settings, as JSON text."
  []
  (json/generate-string (install/hook-entries (bb-edn)) {:pretty true}))

;; --- the page -----------------------------------------------------------------------------------

(defn- shell [title body]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:meta {:name "reus-token" :content (:token @state)}]
          [:title title]
          [:link {:rel "stylesheet" :href "/static/dash.css"}]]
         [:body
          body
          [:script {:src "/static/dash.js"}]]])))

(defn- pill [cls text] [:span {:class (str "pill " cls)} text])

(defn- hook-state
  "A pill for a hook's status: on here, on from another checkout, off."
  [{:keys [on? checkouts error]}]
  (cond error (pill "bad" "unreadable")
        (not on?) (pill "off" "off")
        (contains? checkouts g/home) (pill "on" "on")
        :else (pill "warn" (str "on, from " (str/join ", " (map tilde checkouts))))))

(defn- found-line
  "The one sentence of what was found."
  [c cp hk repos]
  (let [live (:live repos)
        built (reduce + (keep #(:sessions (get repos %)) [:claude-replay :copilot-replay]))]
    (str "Found " (plural (:sessions c) "Claude session" "Claude sessions") " in " (plural (count (:projects c)) "project" "projects")
         ", " (plural (count (:sessions cp)) "Copilot session" "Copilot sessions")
         ". Hooks: Claude " (if (get-in hk [:claude :on?]) "on" "not installed")
         ", Copilot " (if (get-in hk [:copilot :on?]) "on" "not installed")
         ". Live trace: " (if (:absent? live) "none yet" (plural (:sessions live) "session" "sessions"))
         (when (pos? built) (str "; built from old logs: " (plural built "session" "sessions")))
         ".")))

(defn- cards [c cp hk live]
  [:section.cards
   [:div.card
    [:div.card-h "Claude Code"]
    (if (seq (:projects c))
      (list [:div.big (:sessions c) [:small " sessions"]]
            [:div.sub (plural (count (:projects c)) "project" "projects") " · " (plural (:agents c) "agent run" "agent runs")
             " · " (size (:bytes c)) " · newest " (ago (:newest c))])
      [:div.sub "No transcripts found."])
    [:div.path (tilde (fs/path (:dir c) "projects"))]]
   [:div.card
    [:div.card-h "GitHub Copilot " [:small "CLI and IntelliJ"]]
    (if (seq (:sessions cp))
      (list [:div.big (count (:sessions cp)) [:small " sessions"]]
            [:div.sub (str/join " · " (for [[k n] (sort-by key (:by-client cp))] (str n " " k)))
             " · " (:logs cp) " with a log to build from"])
      [:div.sub (if (:exists? cp) "No sessions found." "Copilot is not on this machine.")])
    [:div.path (tilde (fs/path (:home cp) "session-state"))]]
   [:div.card
    [:div.card-h "Recording"]
    [:div.kv [:span "Claude hook"] (hook-state (:claude hk))]
    [:div.kv [:span "Copilot hook"] (hook-state (:copilot hk))]
    [:div.kv [:span "Live trace"] (if (:absent? live) (pill "off" "none yet") (pill "" (plural (:sessions live) "session" "sessions")))]
    [:div.path (tilde (:dir live))]]])

(def ^:private many-bytes "Above this, nothing is picked to build by default." 300e6)
(def ^:private shown "Rows shown before `show all`." 25)

(defn- existing-note
  "What a build would replace."
  [r]
  (if (:absent? r)
    [:span "It makes " [:code (tilde (:dir r))] "."]
    [:span "It replaces " [:code (tilde (:dir r))] " (" (if (= :bench (:layout r)) (str (:mains r) " branches, older layout")
                                                             (plural (:sessions r) "session" "sessions"))
     ", newest " (or (some-> (:newest r) (* 1000) ago) "?") "), once the new one is whole."]))

(defn- claude-picker [c r]
  (let [all? (<= (:bytes c) many-bytes)
        week (- (System/currentTimeMillis) (* 7 86400000))]
    [:div.panel {:data-source "claude"}
     [:div.panel-h [:h3 "From Claude Code's transcripts"]
      [:span.hint "into " [:code (tilde (:dir r))]]]
     (if (empty? (:projects c))
       [:p.empty "No Claude Code transcripts on this machine."]
       (list
        (when-not all?
          [:p.hint "That is a lot (" (size (:bytes c)) "). Pick the projects to build; it takes about a second per MB."])
        [:div.tools
         [:input.filter {:type "search" :placeholder "filter projects" :aria-label "filter projects"}]
         [:button.ghost {:type "button" :data-pick "all"} "all"]
         [:button.ghost {:type "button" :data-pick "none"} "none"]
         [:button.ghost {:type "button" :data-pick "week"} "last 7 days"]]
        [:table.pick
         [:thead [:tr [:th] [:th "project"] [:th.num "sessions"] [:th.num "size"] [:th "last"]]]
         [:tbody
          (for [[i pr] (map-indexed vector (:projects c))]
            [:tr {:class (when (>= i shown) "more") :data-q (str/lower-case (str (:name pr) " " (:cwd pr)))
                  :data-week (when (> (:newest pr) week) "1")}
             [:td [:input {:type "checkbox" :value (:key pr) :data-n (:sessions pr) :data-bytes (:bytes pr)
                           :checked all? :aria-label (str "build " (:name pr))}]]
             [:td [:b (:name pr)] [:div.cwd (tilde (:cwd pr))]]
             [:td.num (:sessions pr) (when (pos? (:agents pr)) [:div.cwd (str "+" (:agents pr) " agents")])]
             [:td.num (size (:bytes pr))]
             [:td (ago (:newest pr))]])]]
        (when (> (count (:projects c)) shown)
          [:button.ghost.showall {:type "button"} (str "show all " (count (:projects c)))])
        [:div.go
         [:button.primary {:type "button" :data-build "claude"} "Build Claude traces"]
         [:span.count]
         [:div.confirm {:hidden true}
          [:p (existing-note r) " Reads the transcripts and your project repos; writes nothing else."]
          [:button.primary {:type "button" :data-build-go "claude"} "Yes, build"]
          [:button.ghost {:type "button" :data-cancel "1"} "cancel"]]]))]))

(defn- copilot-picker [cp r]
  [:div.panel {:data-source "copilot"}
   [:div.panel-h [:h3 "From Copilot's session logs"]
    [:span.hint "into " [:code (tilde (:dir r))]]]
   (if (zero? (:logs cp))
     [:p.empty (if (seq (:sessions cp)) "No Copilot session here has a log (events.jsonl) to build from." "No Copilot sessions on this machine.")]
     (list
      [:table.pick
       [:thead [:tr [:th] [:th "session"] [:th "from"] [:th.num "size"] [:th "last"]]]
       [:tbody
        (for [[i s] (map-indexed vector (:sessions cp))]
          [:tr {:class (str (when (>= i shown) "more") (when-not (:log s) " nolog")) :data-q (str/lower-case (str (:cwd s) " " (:name s)))}
           [:td (when (:log s) [:input {:type "checkbox" :value (:id s) :data-n 1 :data-bytes (:bytes s) :checked true
                                       :aria-label (str "build " (short-id (:id s)))}])]
           [:td [:b (str (fs/file-name (str (:cwd s))))] " " [:code.id (short-id (:id s))]
            [:div.cwd (tilde (:cwd s)) (when (:name s) (str " · " (clip (:name s) 40)))]]
           [:td (:client s) (when-not (:log s) [:div.cwd "no log"])]
           [:td.num (when (:log s) (size (:bytes s)))]
           [:td (ago (:updated s))]])]]
      (when (> (count (:sessions cp)) shown)
        [:button.ghost.showall {:type "button"} (str "show all " (count (:sessions cp)))])
      [:div.go
       [:button.primary {:type "button" :data-build "copilot"} "Build Copilot traces"]
       [:span.count]
       [:div.confirm {:hidden true}
        [:p (existing-note r) " Reads the session logs and your project repos; writes nothing else."]
        [:button.primary {:type "button" :data-build-go "copilot"} "Yes, build"]
        [:button.ghost {:type "button" :data-cancel "1"} "cancel"]]]))])

(defn- job-panel [j]
  [:div#job.job {:data-job (json/generate-string (or j {})) :hidden (nil? j)}
   [:div.job-h [:b#job-title "Building"] [:span#job-state]]
   [:div.bar [:i#job-bar]]
   [:div#job-text.sub]])

(def ^:private repo-titles
  {:live ["Live recording" "every session since the hooks went on"]
   :claude-replay ["From old Claude logs" "built by the button above"]
   :copilot-replay ["From old Copilot logs" "built by the button above"]})

(defn- browse-href [key & {:keys [m p]}]
  (str "/browse?r=" (enc key) (when m (str "&m=" (enc m))) (when p (str "&p=" (enc p)))))

(defn- repo-card [k r]
  (let [key (repo-key k (:dir r))
        [title sub] (get repo-titles k [(:name r) "another trace repo in var/"])]
    [:div.repo {:id (str "repo-" key)}
     [:div.repo-h
      [:div [:h3 title] [:span.hint sub " · " [:code (tilde (:dir r))]]]
      (when-not (:absent? r)
        [:a.button {:href (browse-href key)} "Browse"])]
     (cond
       (:absent? r) [:p.empty (if (= k :live) "Nothing recorded live yet: the hooks (3) record every new session from the moment they are on." "Not built yet.")]
       (= :bench (:layout r)) [:p.sub (plural (:mains r) "branch" "branches") " (a bench or older replay layout); browse it as a whole."]
       (empty? (:projects r)) [:p.empty "No sessions in it yet."]
       :else
       (list
        [:p.sub (plural (:sessions r) "session" "sessions") " in " (plural (count (:projects r)) "project" "projects")
         (when (pos? (:agents r)) (str ", " (plural (:agents r) "agent run" "agent runs"))) ", newest " (ago (some-> (:newest r) (* 1000)))]
        [:table.projects
         [:thead [:tr [:th "project"] [:th.num "sessions"] [:th.num "agent runs"] [:th "newest"]]]
         [:tbody
          (for [[i pr] (map-indexed vector (:projects r))]
            (list
             [:tr {:class (when (>= i shown) "more")}
              [:td [:a {:href (browse-href key :m (:name pr))} [:b (:name pr)]]]
              [:td.num (count (:sessions pr))]
              [:td.num (:agents pr)]
              [:td (ago (some-> (:newest pr) (* 1000)))]]
             [:tr.sessions {:class (when (>= i shown) "more")}
              [:td {:colspan 4}
               [:details
                [:summary (plural (count (:sessions pr)) "session" "sessions")]
                [:ul.sess
                 (for [s (take 60 (:sessions pr))]
                   [:li
                    (if (:id s)
                      [:a {:href (browse-href key :m (:name pr) :p (:id s))} [:code.id (subs (:branch s) (inc (str/last-index-of (:branch s) "/")))]]
                      [:code.id (:branch s)])
                    [:span.when (ago (some-> (:at s) (* 1000)))]
                    (when (:prompts s) [:span.when (plural (:prompts s) "prompt" "prompts")])
                    [:span.ptext (clip (:prompt s) 110)]])]]]]))]]
        (when (> (count (:projects r)) shown)
          [:button.ghost.showall {:type "button"} (str "show all " (count (:projects r)))])))]))

(defn- json-block [text] [:pre.file text])

(defn- hooks-section [hk]
  (let [{:keys [claude copilot]} hk
        cl-on? (:on? claude) co-on? (:on? copilot)
        here? (fn [st] (contains? (:checkouts st) g/home))]
    [:section#hooks.block
     [:h2 [:span.step "3"] "Record from now on " [:small "optional"]]
     [:p.lede "Until now nothing outside reus was touched. These two switches are the only things on this page that write
       outside reus: they put the reus hooks into Claude Code and Copilot, so every new session is recorded into "
      [:code (tilde g/*repo*)] " as it happens. Off takes them out again; what was recorded stays."]
     [:div.hooks
      [:div.panel.outside
       [:div.panel-h [:h3 "Claude Code"] (hook-state claude)]
       [:p.sub "Writes " [:code (tilde (:file claude))]
        (if (:exists? claude) " (the old one is kept as settings.json.bak; your other settings and hooks stay)." " (there is none yet: it is made).")]
       [:details
        [:summary "What on writes"]
        [:p.sub "Seven hook entries, each running this checkout's " [:code "bb -m trace.hook"] ":"]
        (json-block (hook-lines))
        [:p.sub "It also makes the live trace repo " [:code (tilde g/*repo*)] " when it is not there."]]
       [:details
        [:summary "What off writes"]
        [:p.sub "The same file without the entries whose command names " [:code "trace.hook"] "; everything else is kept."]]
       [:div.go
        (if (and cl-on? (here? claude))
          [:button.outside {:type "button" :data-hook "claude" :data-to "off"} "Turn off the Claude hook"]
          [:button.outside {:type "button" :data-hook "claude" :data-to "on"} (if cl-on? "Point the Claude hook at this checkout" "Turn on the Claude hook")])
        (when (and cl-on? (not (here? claude)))
          [:button.outside.ghost {:type "button" :data-hook "claude" :data-to "off"} "Turn it off"])
        [:div.confirm {:hidden true}
         [:p "This writes " [:code (tilde (:file claude))] "."]
         [:button.outside {:type "button" :data-hook-go "1"} "Yes, write it"]
         [:button.ghost {:type "button" :data-cancel "1"} "cancel"]]]]
      [:div.panel.outside
       [:div.panel-h [:h3 "GitHub Copilot"] (hook-state copilot)]
       [:p.sub "Writes in " [:code (tilde (:home copilot))] " (the CLI and the IntelliJ plugin read the same hooks)."
        (when-not (:exists? copilot) " Copilot is not on this machine yet: it would make the folder.")]
       [:details
        [:summary "What on writes"]
        (for [[f text] (:writes (ci/plan :install found/*copilot-home*))]
          (list [:p.sub [:code (tilde f)]]
                (json-block (if (str/ends-with? f ".json") text (clip text 600)))))]
       [:details
        [:summary "What off deletes"]
        [:p.sub [:code (tilde (:file copilot))] " and " [:code (tilde (fs/path (:home copilot) "skills" "reus-compare"))] ", nothing else."]]
       [:div.go
        (if (and co-on? (here? copilot))
          [:button.outside {:type "button" :data-hook "copilot" :data-to "off"} "Turn off the Copilot hooks"]
          [:button.outside {:type "button" :data-hook "copilot" :data-to "on"} (if co-on? "Point the Copilot hooks at this checkout" "Turn on the Copilot hooks")])
        (when (and co-on? (not (here? copilot)))
          [:button.outside.ghost {:type "button" :data-hook "copilot" :data-to "off"} "Turn them off"])
        [:div.confirm {:hidden true}
         [:p "This writes in " [:code (tilde (:home copilot))] "."]
         [:button.outside {:type "button" :data-hook-go "1"} "Yes, write it"]
         [:button.ghost {:type "button" :data-cancel "1"} "cancel"]]]]]
     [:div#hook-out.out {:hidden true}]]))

(defn- running-section [repos]
  (let [ws (found/workers)
        health (found/health)
        viewers (:viewers @state)
        live (:live repos)]
    [:section#running.block
     [:h2 [:span.step "4"] "What is running, and is it well"]
     [:div.grid2
      [:div.panel
       [:div.panel-h [:h3 "Running"]]
       [:div.kv [:span "This dashboard"] [:code (dash-url)]]
       (if (seq viewers)
         (for [[d {:keys [port]}] (sort-by (comp :port val) viewers)]
           [:div.kv [:span "Viewer of " [:code (tilde d)]] [:a {:href (str "http://127.0.0.1:" port "/")} (str "127.0.0.1:" port)]])
         [:div.kv [:span "Viewers"] [:span.hint "start when you press Browse"]])
       [:div.kv [:span "bb trace:gui on :7360"] (if (found/port-open? 7360) (pill "on" "running") (pill "off" "not running"))]
       [:div.kv [:span "Trace worker"]
        (if (seq ws)
          [:span (for [w ws] [:span.wk (pill "on" (str "pid " (:pid w))) " " [:code.cwd (tilde (:checkout w))]])]
          [:span (pill "off" "idle") [:span.hint " the hook starts it when events come"]])]
       (when-let [t (and (not (:absent? live)) (found/worker-log))]
         [:div.kv [:span "Worker last wrote"] [:span (ago t)]])]
      [:div.panel
       [:div.panel-h [:h3 "Health"]
        (cond (nil? health) (pill "off" "no live trace")
              (empty? health) (pill "on" "trace ok")
              :else (pill "bad" (plural (count health) "problem" "problems")))]
       (if (seq health)
         [:ul.problems (for [[label _ reason] health] [:li [:b label] ": " reason])]
         [:p.sub (if (nil? health) (list "No live trace repo here yet (" [:code (tilde g/*repo*)] ").") "The hooks ran, the worker kept up, nothing failed.")])
       [:p.sub "Same as " [:code "bb trace:health"] " in a terminal."]
       (when-not (:absent? live)
         [:p.links
          [:a {:href (str "/browse?r=live&path=" (enc "/runs"))} "Runs: the same task on several models"]
          [:a {:href (str "/browse?r=live&path=" (enc "/calls"))} "Calls: one model call explained"]])]]]))

(defn page
  "The dashboard, as HTML: everything found now."
  []
  (let [c (found/claude)
        cp (found/copilot)
        hk (found/hooks)
        repos (found/repos)
        live (:live repos)]
    (shell "reus"
           [:div.wrap
            [:header.top
             [:div.brand "reus" [:span " records what AI coding agents do, as git history"]]
             [:div.where "running from " [:code (tilde g/home)]]]
            [:p.found (found-line c cp hk repos)
             [:span.ro " Nothing has been changed: this page only looks until you press a button."]
             " " [:a.again {:href "/"} "look again"]]
            (cards c cp hk live)
            [:section#build.block
             [:h2 [:span.step "1"] "Build traces from the old logs"]
             [:p.lede "Every session Claude Code and Copilot already ran is in their own logs. This turns them into git
               history, one commit per tool call with your files as they were at that moment. It only reads the logs and your
               project repos, writes only into reus' own " [:code "var/"] ", and turns no hook on."]
             (job-panel (job))
             [:div.grid2
              (claude-picker c (:claude-replay repos))
              (copilot-picker cp (:copilot-replay repos))]]
            [:section#browse.block
             [:h2 [:span.step "2"] "Browse what is recorded"]
             [:p.lede "Each trace repo opens in the trace viewer: per project its sessions, per prompt every tool call with
               its diff and output."]
             (for [k [:live :claude-replay :copilot-replay]] (repo-card k (get repos k)))
             (let [others (remove (comp keyword? key) repos)]
               (when (seq others)
                 [:details.others
                  [:summary (plural (count others) "other trace repo" "other trace repos") " in " [:code "var/"]]
                  (for [[k r] (sort-by key others)] (repo-card k r))]))]
            (hooks-section hk)
            (running-section repos)
            [:footer "reus · " [:code "bb gui"] " · Ctrl-C in its terminal stops it and every viewer it started"]])))

;; --- http ---------------------------------------------------------------------------------------

(defn- query [s]
  (into {} (for [kv (str/split (or s "") #"&") :let [[k v] (str/split kv #"=" 2)] :when (seq k)]
             [k (URLDecoder/decode (or v "") "UTF-8")])))

(defn- json-resp [status m]
  {:status status :headers {"content-type" "application/json" "cache-control" "no-store"} :body (json/generate-string m)})

(defn- local-host? [req]
  (let [host (str (get-in req [:headers "host"]))]
    (boolean (re-matches #"(127\.0\.0\.1|localhost)(:\d+)?" host))))

(defn- allowed?
  "A POST from this run's page: its token in the x-reus-token header, a local Host."
  [req]
  (and (local-host? req)
       (some? (:token @state))
       (= (:token @state) (get-in req [:headers "x-reus-token"]))))

(defn- body-json [req] (try (json/parse-string (slurp (:body req))) (catch Exception _ {})))

(defn handler [{:keys [uri request-method] :as req}]
  (let [q (query (:query-string req))]
    (try
      (cond
        (not (local-host? req)) {:status 403 :body "forbidden"}

        (and (= uri "/") (= :get request-method))
        {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body (page)}

        (= uri "/x/ping") {:status 200 :headers {"content-type" "text/plain"} :body "reus gui"}

        (= uri "/x/job") (json-resp 200 (or (job) {}))

        (= uri "/browse")
        (if-let [d (repo-dir (get q "r"))]
          (if (fs/exists? (fs/path d ".git"))
            (let [port (viewer! d)
                  path (cond (get q "p") (str "/p/" (enc (get q "p")) "?m=" (enc (get q "m" "")))
                             (get q "m") (str "/?m=" (enc (get q "m")))
                             (#{"/runs" "/calls"} (get q "path")) (get q "path")
                             :else "/")]
              {:status 302 :headers {"location" (str "http://127.0.0.1:" port path)}})
            {:status 404 :body "that trace repo is not built yet"})
          {:status 404 :body "no such trace repo"})

        (and (= uri "/x/build") (= :post request-method))
        (if-not (allowed? req)
          (json-resp 403 {:error "not from this dashboard"})
          (let [{:strs [source keys]} (body-json req)
                source (#{"claude" "copilot"} source)]
            (cond (nil? source) (json-resp 400 {:error "source: claude or copilot"})
                  (empty? keys) (json-resp 400 {:error "nothing picked"})
                  :else (if-let [j (start-build! (keyword source) (map str keys))]
                          (json-resp 202 j)
                          (json-resp 409 {:error "a build is running" :job (job)})))))

        (and (= uri "/x/hooks") (= :post request-method))
        (if-not (allowed? req)
          (json-resp 403 {:error "not from this dashboard"})
          (let [{:strs [which to]} (body-json req)
                which (#{"claude" "copilot"} which) to (#{"on" "off"} to)]
            (if (and which to)
              (json-resp 200 {:out (switch-hooks! (keyword which) (keyword to))})
              (json-resp 400 {:error "which: claude|copilot, to: on|off"}))))

        (#{"/static/dash.css" "/static/dash.js"} uri)
        {:status 200
         :headers {"content-type" (if (str/ends-with? uri ".css") "text/css" "text/javascript") "cache-control" "no-store"}
         :body (slurp (io/resource (str "trace/" (fs/file-name uri))))}

        :else {:status 404 :body "not found"})
      (catch Throwable e
        (binding [*out* *err*] (println uri (ex-message e)))
        {:status 500 :headers {"content-type" "text/plain"} :body (str "error: " (ex-message e))}))))

(defn start!
  "Serve the dashboard on 127.0.0.1:port (a new token for its buttons); the stop fn, which stops the viewers it
  started too."
  [port]
  (let [token (str (java.util.UUID/randomUUID))
        stop (srv/run-server #'handler {:ip "127.0.0.1" :port port})]
    (swap! state assoc :token token :port port)
    (fn []
      (doseq [{:keys [stop]} (vals (:viewers @state))] (stop))
      (swap! state assoc :viewers {})
      (stop))))

(defn- reus-there? [port]
  (try (= "reus gui" (slurp (str "http://127.0.0.1:" port "/x/ping"))) (catch Exception _ false)))

(defn open-browser!
  "Open url in the browser: $BROWSER (a command, the url appended), else `open` (macOS) or `xdg-open`."
  [url]
  (let [b (System/getenv "BROWSER")
        cmd (cond (not (str/blank? b)) ["sh" "-c" (str b " \"$1\"") "sh" url]
                  (fs/which "open") ["open" url]
                  (fs/which "xdg-open") ["xdg-open" url])]
    (if cmd
      (try (p/process {:out :discard :err :discard} cmd) true (catch Exception _ false))
      false)))

(defn -main [& argv]
  (let [opts (loop [[a b & more :as as] argv o {}]
               (cond (empty? as) o
                     (= a "--port") (recur more (assoc o :port (parse-long (str b))))
                     (= a "--no-open") (recur (rest as) (assoc o :no-open true))
                     :else (do (println "usage: bb gui [--port N] [--no-open]") (System/exit 1))))
        want (or (:port opts) default-port)]
    (if (and (found/port-open? want) (reus-there? want))
      (let [url (str "http://127.0.0.1:" want "/")]
        (println "reus gui is already running on" url)
        (when-not (:no-open opts) (open-browser! url)))
      (let [port (if (found/port-open? want) (first (remove found/port-open? (range (inc want) (+ want 100)))) want)
            _ (start! port)
            url (dash-url)]
        (println (str "reus gui on " url))
        (println "  it only looks until you press a button; Ctrl-C stops it")
        (when-not (or (:no-open opts) (open-browser! url))
          (println "  open that address in a browser"))
        @(promise)))))
