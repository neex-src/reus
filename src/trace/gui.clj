(ns trace.gui
  "Browser view of a trace repo: the central one trace.record writes, or a bench
  repo written by hooks/trace.bb.

  bb trace:gui [repo] [port]    central repo on :7360, any other on :7357

  Bench repo: prompt NN of every main (lame, sane, ...) side by side. Central repo:
  one project at a time (the picker in the top bar), newest prompt first.
  Git is read on every request (per-branch results cached by tip), and the page
  polls /x/tips to offer, or with 'follow live' do, the refresh itself.

  Routes
    /?m=a                the newest prompt of a (default: see default-mains)
    /p/<session8>@NN?m=a    prompt NN of a session of a
    /files?m=a           every file a edited
    /file?m=a&path=P     every edit of P, in order: the file's evolution
    /fns?m=a[&file=F]    every Clojure definition a ever had (the fn layer, trace.fns)
    /fn?m=a&path=F/D     every version of definition D of file F, oldest first: its evolution
    /x/show?r=REV        git cat-file -p REV (a blob sha, or <commit>:<path>)
    /x/diff?c=SHA[&path=P]   that commit's change outside reads/ and doto.md
    /x/net?m=a&p=NN      the net change of a prompt
    /x/bdiff?a=SHA&b=SHA  git diff of two blobs (a definition's versions)
    /x/tips?m=a          a's branch tips, running prompts, newest prompt: the live poll"
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as srv]
            [trace.git :as g :refer [git]]
            [trace.agents :as agents]
            [trace.fnlog :as fl]
            [trace.read :refer [tips mains prompt-nos prompt-data all-prompts live-branches ext-lang central?]])
  (:import [java.net URLDecoder URLEncoder]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- newest-session
  "The project whose session branch has the newest tip commit."
  []
  (->> (str/split-lines (git "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)" "refs/heads"))
       (some #(second (re-matches #"(.+)/s/[^/]+" %)))))

(defn- agent-id? [id] (str/includes? (first (str/split id #"@")) "-"))

(defn- session-ids
  "Ids of the prompts of the given projects' sessions (agent runs nest under their Agent step), oldest first."
  [tips ms]
  (remove agent-id? (mapcat #(prompt-nos tips %) ms)))

(defn- pno [id] (second (str/split id #"@" 2)))
(defn- sess-of [id] (first (str/split id #"@" 2)))

(defn- parent-id
  "The session prompt an id belongs to: itself, or for an agent run the prompt it was started in."
  [id]
  (str (first (str/split (sess-of id) #"-")) "@" (first (str/split (str (pno id)) #"-"))))

(defn- chip-text
  "A prompt id as a short chip: NN and the start of its session."
  [id]
  (let [p (parent-id id)] (str (pno p) " · " (subs (sess-of p) 0 (min 4 (count (sess-of p)))))))

(defn- live-in
  "The running prompt ids (live-branches) of projects ms: those whose session or agent branch is one of theirs."
  [tips ms]
  (set (filter (fn [id] (let [s (sess-of id)]
                          (some #(get tips (str % (if (str/includes? s "-") "/a/" "/s/") s)) ms)))
               (live-branches))))

(defn- running?
  "Is the session prompt id, or an agent run started in it, among the live ids?"
  [live id]
  (or (contains? live id)
      (let [pre (str (sess-of id) "-") no (str (pno id) "-a")]
        (some #(and (str/starts-with? (sess-of %) pre) (str/starts-with? (str (pno %)) no)) live))))

(defn- agent-ids
  "{agent id, prompt id} of the agent runs of a session (by its prompt id) in project main."
  [tips main id]
  (let [pre (str (sess-of id) "-")]
    (into {} (keep #(let [s (sess-of %)] (when (str/starts-with? s pre) [(subs s (count pre)) %])))
          (prompt-nos tips main))))

(defn- stamp [secs]
  (when secs
    (.format (DateTimeFormatter/ofPattern "MMM d, HH:mm")
             (.atZone (Instant/ofEpochSecond secs) (ZoneId/systemDefault)))))

;; --- html -----------------------------------------------------------------

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))
(defn- mmss [secs]
  (if (and secs (>= secs 0))
    (if (>= secs 3600)
      (format "%d:%02d:%02d" (quot secs 3600) (quot (mod secs 3600) 60) (mod secs 60))
      (format "%d:%02d" (quot secs 60) (mod secs 60)))
    ""))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))
(defn- lines-text [n] (str n (if (= 1 n) " line" " lines")))
(defn- first-line [s] (first (str/split-lines (str s))))

(defn- prompt-title
  "What a prompt that Claude Code wrote, not the user, says: a task notification by its summary, an agent's
  hand-back by its agent; nil for a typed prompt."
  [text]
  (let [t (str/triml (str text))
        tag (fn [k] (some-> (re-find (re-pattern (str "(?s)<" k ">(.*?)</" k ">")) t) second str/trim))]
    (cond
      (str/starts-with? t "<task-notification>")
      (str "Notification: " (or (tag "summary") (str "task " (tag "task-id") " " (tag "status"))))
      (str/starts-with? t "<agent-message ")
      (str "Agent " (second (re-find #"^<agent-message from=\"([^\"]+)\"" t)) " handed back its report"))))

(defn- kind-class [s] (str "k-" (if (:err s) "err" (name (:kind s)))))

(defn- label [{:keys [kind call read-file files cmd]}]
  (let [{:keys [tool input code]} call
        target (or (get input "file_path") (get input "path"))]
    (cond
      (and (= kind :read) read-file (not code)) read-file
      code (first-line code)
      (= "Agent" tool) (str (get input "description"))
      target (str target (when-let [f (or (get input "form_identifier") (get input "name_pattern") (get input "pattern"))]
                           (str "  ·  " f)))
      (seq files) (str/join ", " (map :path files))
      :else (clip (str (or (get input "pattern") (get input "query") cmd)) 120))))

(defn- tape [steps & {:keys [href-fn cls]}]
  [:div {:class (str "tape " cls)}
   (for [s steps]
     [(if href-fn :a :span)
      {:class (str "cell " (kind-class s))
          :href (when href-fn (href-fn s))
          :title (str (:i s) " · " (name (:kind s)) (when (:err s) " · failed") " · " (clip (label s) 80))}])])

(defn- code-block [text lang & [cls]]
  [:pre {:class (str "code " cls)} [:code {:class (str "language-" lang) :data-hl "1"} text]])

(defn- lazy [title src & {:keys [kind lang open? cls]}]
  [:details (cond-> {:class (str "lazy " cls) :data-src src :data-kind (or kind "text")}
              lang (assoc :data-lang lang)
              open? (assoc :open true :data-auto "1"))
   [:summary title]
   [:div.lazy-body [:div.loading "loading…"]]])

(defn- input-view [{:keys [input]}]
  (let [lang (ext-lang (or (get input "file_path") (get input "path")))
        long-keys #{"content" "code" "old_string" "new_string" "new_content"}]
    [:div.input
     [:table.kv
      (for [[k v] input :when (not (long-keys k))]
        [:tr [:th k] [:td (if (string? v) v (json/generate-string v))]])]
     (when-let [o (get input "old_string")] [:div.pair [:div.tag.del "old"] (code-block o lang "old")])
     (when-let [n (get input "new_string")] [:div.pair [:div.tag.add "new"] (code-block n lang "new")])
     (for [k ["content" "new_content" "code"] :let [v (get input k)] :when v]
       [:div.pair [:div.tag k] (code-block v (if (= k "code") "clojure" lang))])]))

(defn- agent-badges
  "Model, effort and run time of a subagent step."
  [{:keys [model effort ms running?]}]
  (list
   (when model [:span.ab.model (str/replace model #"^claude-" "")])
   (when effort [:span.ab.effort effort])
   (when ms [:span.ab.dur (mmss (quot ms 1000))])
   (when running? [:span.running [:i.pulse] "running"])))

(declare step-view)

(defn- agent-run
  "The recorded history of a subagent run (its own branch), nested under the Agent step, or under
  its prompt when no Agent step names it (title given)."
  [tips main nn s session & [title]]
  (when-let [d (and (:agent-id s)
                    (some->> (get (agent-ids tips main nn) (:agent-id s)) (prompt-data tips main)))]
    [:div.agent-run
     [:div.reply-h (or title (str "Agent run · " (count (:steps d)) " steps"))]
     (when title [:div.md.brief (:prompt d)])
     [:ol.steps.nested (for [s2 (:steps d)] (step-view main (:nn d) (:t0 d) s2 session tips))]
     (when (:reply d) [:details [:summary "Agent's last message"] [:div.md (:reply d)]])]))

(defn- step-view [main nn t0 s session tips]
  (let [id (str main "-" nn "-" (:i s))
        {:keys [tool code lang]} (:call s)
        agent (when (= "Agent" tool) (agents/info session (:agent-id s) (:input (:call s))))]
    [:li {:id id :class (str "step " (kind-class s)) :data-kind (if (:err s) "err" (name (:kind s)))}
     [:details.step-d
      [:summary
       [:span.i (:i s)]
       [:span.kind (if (:err s) "failed" (name (:kind s)))]
       [:span.tool tool]
       [:span.lbl (clip (label s) 160)]
       [:span.meta
        (when agent (agent-badges agent))
        (when (= :edit (:kind s)) [:span.delta [:b.add (str "+" (:add s))] " " [:b.del (str "−" (:del s))]])
        (when (:lines s) [:span.lines (lines-text (:lines s))])
        [:span.t (str "+" (mmss (- (:at s) t0)))]]]
      [:div.step-body
       (when agent
         [:div.agent
          [:span.ab.type (:type agent)]
          (agent-badges agent)
          [:span.ab (str (:calls agent) " tool calls")]
          [:code.sha (:agent-id s)]])
       (if code
         (code-block code lang "cmd")
         (input-view (:call s)))
       (when (string? (:err s))
         [:div.errbox [:div.tag.err "error"] (code-block (:err s) "plaintext" "err")])
       (case (:kind s)
         :edit (if (empty? (:files s))
                (when (:result s)
                  (lazy (str "Output · " (lines-text (:lines s)))
                        (str "/x/show?r=" (:result s)) :kind "text"))
                (list
                (lazy (str "Diff · " (str/join ", " (map :path (:files s))))
                      (str "/x/diff?c=" (:sha s)) :kind "diff" :open? true)
                [:div.after
                 (for [f (:files s)]
                   (if (:gone f)
                     [:div.note "Deleted by this step: " [:span.file (:path f)]]
                     (lazy (str "File after this edit · " (:path f))
                         (str "/x/show?r=" (enc (str (:sha s) ":" (:path f))))
                         :kind "code" :lang (ext-lang (:path f)) :cls "nested")))]))
         :read (list
                (if (:result s)
                  (lazy (str "Output · " (lines-text (:lines s)))
                        (str "/x/show?r=" (:result s)) :kind "text" :open? true)
                  [:div.note (if (:silent s) "It printed nothing." "No output recorded.")])
                (when (:unheld s)
                  [:div.note "The file itself is not in the tree recorded for this step (an ignored file, or one outside the worktree): the output above is what was read."])
                (when (:blob s)
                 (lazy [:span "File as it was when read · " [:span.file (:read-file s)]]
                      (str "/x/show?r=" (enc (:blob s))) :kind "code" :lang (ext-lang (:read-file s)) :cls "nested blob")))
         :run (if (:result s)
                (lazy (str "Output · " (lines-text (:lines s)))
                      (str "/x/show?r=" (:result s)) :kind "text" :open? true)
                [:div.note (if (:silent s) "It printed nothing." "No output recorded.")])
         nil)
       (agent-run tips main nn s session)
       [:div.commit "commit " [:code (subs (:sha s) 0 10)] " · " (:subject s)]]]]))

(defn- note-view
  "A task notification that reached the session while this prompt was open or after
  its reply, before the next prompt."
  [t0 {:keys [at task status summary result]} session]
  (let [agent (agents/info session task {})]
    [:li.note-ev {:class (when (= "failed" status) "failed")}
     [:span.t (str "+" (mmss (- at t0)))]
     [:span.st status]
     [:span.lbl (clip summary 200)]
     (when agent (agent-badges agent))
     (when-not (str/blank? result)
       [:details [:summary "result · " (lines-text (count (str/split-lines result)))] (code-block result "plaintext")])]))

(defn- notes-of
  "The session's notifications between this prompt's start and the next prompt of the same session."
  [tips d]
  (when-let [session (:session d)]
    (let [end (->> (all-prompts tips (:main d))
                   (filter #(and (= session (:session %)) (not (agent-id? (:nn %))) (> (:t0 %) (:t0 d))))
                   (map :t0) sort first)]
      (filter #(and (>= (:at %) (:t0 d)) (or (nil? end) (< (:at %) end)))
              (agents/notifications session)))))

(defn- counts [steps]
  (merge {:read 0 :edit 0 :run 0 :err 0} (frequencies (map #(if (:err %) :err (:kind %)) steps))))

(def ^:private kinds [[:read "reads"] [:edit "edits"] [:run "runs"] [:err "failed"]])

(defn- page [title body & {:keys [watch]}]
  (str
   "<!doctype html>"
   (h/html
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:title title]
      [:link {:rel "preconnect" :href "https://fonts.googleapis.com"}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;600&display=swap"}]
      [:link {:rel "stylesheet" :href "/static/app.css"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/clojure.min.js"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/marked/12.0.2/marked.min.js"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/dompurify/3.1.6/purify.min.js"}]]
     [:body {:data-watch (json/generate-string (or watch {}))}
      body
      [:script {:src "/static/shell.js"}]
      [:script {:src "/static/app.js"}]]])))

(defn- m-param [ms] (str "m=" (str/join "," (map enc ms))))

(defn- default-mains
  "All mains of a bench repo (to compare them); in the central repo, the project
  whose prompt is newest."
  [tips]
  (if (central?)
    (some-> (newest-session) vector)
    (mains tips)))

(defn- topbar [tips current ms]
  [:header.top
   [:a.brand {:href "/"} "reus trace"]
   [:span.repo g/*repo*]
   (when (central?)
     [:select.proj {:aria-label "Project" :data-nav "1"}
      (for [m (mains tips)] [:option {:value (str "/?m=" (enc m)) :selected (= [m] (vec ms))} m])])
   [:nav
    [:a {:href (str "/?" (m-param ms)) :class (when (= current :prompts) "on")} "Prompts"]
    (for [m ms]
      (list [:a {:href (str "/files?m=" (enc m)) :class (when (= current m) "on")} (str "Files · " m)]
            (when (fl/on? m) [:a {:href (str "/fns?m=" (enc m)) :class (when (= current [:fns m]) "on")} (str "Functions · " m)])))]
   [:div.live
    [:span#livestate.state "watching"]
    [:label {:title "Show each command of a shell step with the output it printed. A guess: shell output has no separators, so commands are told apart by echo markers and by the shape of their lines."}
     [:input#splitout {:type "checkbox"}] " output per command (experimental)"]
    [:label [:input#follow {:type "checkbox"}] " follow live"]]])

(defn- prompt-index [tips ms current]
  (let [live (live-in tips ms)
        nos (session-ids tips ms)]
    [:aside.index
     [:div.legend (for [[k t] kinds] [:span {:class (str "lg k-" (name k))} [:i] t])]
     [:ol
      (for [nn (reverse nos)
            :let [ds (keep #(prompt-data tips % nn) ms)
                  text (some :prompt ds)]]
        [:li
         [:a {:class (str "pr" (when (= nn current) " cur")) :href (str "/p/" (enc nn) "?" (m-param ms))}
          [:span.nn (pno nn)]
          [:span.ptext (clip (or (prompt-title text) text) 90)
           (when (central?) [:span.when (some-> (some :t0 ds) stamp) " · " [:span.sess (sess-of nn)]])]
          [:span.tapes
           (for [d ds]
             [:span.trow
              [:span.mname (:main d) (when (running? live nn) [:i.pulse])]
              (tape (take 64 (:steps d)) :cls "mini")
              [:span.cnt (count (:steps d)) (when (> (count (:steps d)) 64) [:em "+"])]])]]])]]))

(defn- prompt-page [tips nn ms]
  (let [all (mains tips)
        ms (or (seq (filter (set all) ms)) (default-mains tips) all)
        central (central?)
        ds (keep #(prompt-data tips % nn) ms)
        text (some :prompt ds)
        live (live-in tips ms)]
    (page (str "Prompt " (pno nn) " · trace")
          [:div.app
           (topbar tips :prompts ms)
           [:div.shell
            (prompt-index tips ms nn)
            [:main
             [:section.head
              [:div.pno [:span "Prompt"] [:b (pno nn)]]
              (if-let [t (prompt-title text)]
                [:div.prompt-text [:p t] [:details [:summary "as Claude Code wrote it"] (code-block text "xml")]]
                [:div.md.prompt-text text])
              (for [d ds :when (:session d)]
                [:p.where (:main d) " · " (stamp (:t0 d)) " · in " [:code (:cwd d)] " · session " [:code (subs (:session d) 0 (min 8 (count (:session d))))]])
              [:table.compare
               [:thead [:tr [:th] [:th "steps"] (for [[_ t] kinds] [:th t]) [:th "lines ±"] [:th "time"]]]
               [:tbody
                (for [d ds :let [c (counts (:steps d))]]
                  [:tr [:th (:main d)]
                   [:td [:b (count (:steps d))]]
                   (for [[k _] kinds] [:td {:class (str "c-" (name k))} (get c k 0)])
                   [:td [:span.add (str "+" (reduce + (map :add (:steps d))))] " " [:span.del (str "−" (reduce + (map :del (:steps d))))]]
                   [:td (mmss (- (:t1 d) (:t0 d)))
                    (when (running? live nn) [:span.running [:i.pulse] "running"])]])]]
              [:div.tools
               [:div.filters "Show "
                (for [[k t] kinds] [:label {:class (str "f k-" (name k))} [:input {:type "checkbox" :checked true :data-filter (name k)}] t])]
               [:div.btns
                [:button {:type "button" :data-act "open"} "Expand all"]
                [:button {:type "button" :data-act "close"} "Collapse all"]
                (when (and (not central) (> (count all) 1))
                  (for [m all] [:a.solo {:href (str "/p/" (enc nn) "?m=" (enc m))} (str "Only " m)]))
                (when (and (not central) (< (count ds) (count all))) [:a.solo {:href (str "/p/" (enc nn))} "All side by side"])]]]
             [:div.cols {:style (str "--n:" (max 1 (count ds)))}
              (for [d ds :let [{:keys [main steps t0]} d]]
                [:section.col
                 [:h2 main [:small (count steps) " steps · " (mmss (- (:t1 d) t0))]]
                 (tape steps :href-fn #(str "#" main "-" nn "-" (:i %)) :cls "big")
                 (lazy "Net change of this prompt" (str "/x/net?m=" (enc main) "&p=" (enc nn)) :kind "diff" :cls "net")
                 [:ol.steps (for [s steps] (step-view main nn t0 s (:session d) tips))]
                 (let [nested (set (keep :agent-id steps))
                       loose (remove (comp nested key) (filter #(= (pno (val %)) (str (pno nn) "-a" (key %))) (agent-ids tips main nn)))]
                   (when (seq loose)
                     [:div.events
                      [:div.reply-h (str "Agent runs with no Agent step here · " (count loose))]
                      (for [[aid _] loose]
                        (agent-run tips main nn {:agent-id aid} (:session d)
                                   (str "Agent " aid (when-let [a (agents/info (:session d) aid {})] (str " · " (:type a))))))]))
                 (when-let [ns (seq (notes-of tips d))]
                   [:div.events
                    [:div.reply-h (str "Background events · " (count ns))]
                    [:ol (for [n ns] (note-view t0 n (:session d)))]])
                 [:div.reply
                  [:div.reply-h "Reply"]
                  (if (:reply d)
                    [:div.md (:reply d)]
                    [:div.note (if (running? live nn) "Still working…" "No reply recorded.")])]])]]]]
          :watch {:mains ms})))

(defn- files-page [tips main]
  (let [ds (all-prompts tips main)
        edits (for [d ds s (:steps d) :when (= :edit (:kind s)) f (:files s)] [(:path f) d f])
        rows (->> (group-by first edits)
                  (map (fn [[path es]]
                         {:path path :n (count es)
                          :add (reduce + (keep #(:add (nth % 2)) es)) :del (reduce + (keep #(:del (nth % 2)) es))
                          :prompts (distinct (map #(:nn (second %)) es))}))
                  (sort-by (juxt (comp - :n) :path)))]
    (page (str "Files · " main)
          [:div.app
           (topbar tips main [main])
           [:main.wide
            [:h1 "Files " main " edited"]
            [:p.sub (count rows) " files, " (count edits) " edits over " (count ds) " prompts. Pick one to see every edit in order."]
            [:table.files
             [:thead [:tr [:th "file"] [:th "edits"] [:th "lines ±"] [:th "in prompts"]]]
             [:tbody
              (for [r rows]
                [:tr
                 [:td [:a {:href (str "/file?m=" (enc main) "&path=" (enc (:path r)))} (:path r)]]
                 [:td.num (:n r)]
                 [:td.num [:span.add (str "+" (:add r))] " " [:span.del (str "−" (:del r))]]
                 [:td (for [nn (distinct (map parent-id (:prompts r)))] [:a.chip {:href (str "/p/" (enc nn) "?m=" (enc main)) :title (str "session " (sess-of nn))} (chip-text nn)])]])]]]])))

(defn- file-page [tips main path]
  (let [ds (all-prompts tips main)
        edits (for [d ds s (:steps d) :when (some #(= path (:path %)) (:files s))] [d s])
        others (when-not (central?) (remove #{main} (mains tips)))
        now (or (some-> (last edits) second :sha) (get tips (str main "/main")))]
    (page (str path " · " main)
          [:div.app
           (topbar tips main [main])
           [:main.wide
            [:h1.path path]
            [:p.sub (count edits) " edits by " main ". "
             (when (and (fl/on? main) (re-find #"\.(clj|cljs|cljc|bb)$" path)) [:a {:href (str "/fns?m=" (enc main) "&file=" (enc path))} "Its definitions, one by one. "])
             (for [o others] [:a {:href (str "/file?m=" (enc o) "&path=" (enc path))} (str "Same file in " o)])]
            (lazy "The file after its last edit here"
                  (str "/x/show?r=" (enc (str now ":" path))) :kind "code" :lang (ext-lang path) :cls "nested")
            [:ol.evo
             (for [[d s] edits]
               [:li {:class (str "evo-item " (kind-class s))}
                [:div.evo-h
                 [:a.chip {:href (str "/p/" (enc (parent-id (:nn d))) "?m=" (enc main) "#" (enc (str main "-" (:nn d) "-" (:i s))))}
                  (str "prompt " (chip-text (:nn d)) (when (not= (:nn d) (parent-id (:nn d))) " · agent") " · step " (:i s))]
                 [:span.tool (:tool (:call s))]
                 [:span.ptext (clip (or (prompt-title (:prompt d)) (:prompt d)) 110)]
                 [:span.delta [:b.add (str "+" (:add s))] " " [:b.del (str "−" (:del s))]]]
                (lazy "Diff" (str "/x/diff?c=" (:sha s) "&path=" (enc path)) :kind "diff" :open? true)])]]])))

;; --- the fn layer -----------------------------------------------------------

(defn- fn-split
  "A definition path <file>/<name> as [file name], the name as written (trace.fns writes / as %2F)."
  [path]
  (let [i (str/last-index-of path "/")]
    [(subs path 0 i) (str/replace (subs path (inc i)) "%2F" "/")]))

(defn- fns-page [tips main only]
  (let [ix (fl/index main)
        now (fl/on-main main)
        files (->> (group-by (comp first fn-split key) ix)
                   (filter #(or (nil? only) (= only (key %))))
                   (sort-by (fn [[_ es]] (- (apply max (map (comp :last val) es))))))]
    (page (str "Functions · " main)
          [:div.app
           (topbar tips [:fns main] [main])
           [:main.wide
            [:h1 (if only [:span "Definitions of " [:span.path only]] (str "Functions of " main))]
            [:p.sub (reduce + (map (comp count val) files)) " definitions in " (count files) " files, "
             (count (filter now (mapcat (comp keys val) files))) " of them on main now. "
             "Each one's versions come from the fn layer (refs/fn/" main "/…): one version per change, from the call that made it. "
             (when only [:a {:href (str "/fns?m=" (enc main))} "All files."])]
            [:input#fnq.fnq {:type "search" :placeholder "filter: file or name" :autofocus true}]
            (for [[file es] files]
              [:section.fnfile
               [:h3 [:a {:href (str "/file?m=" (enc main) "&path=" (enc file))} file]]
               [:table.files
                [:tbody
                 (for [[path {:keys [n last]}] (sort-by (comp - :last val) es)
                       :let [[_ nm] (fn-split path)]]
                   [:tr {:data-q (str/lower-case path) :class (when-not (now path) "gone")}
                    [:td [:a {:href (str "/fn?m=" (enc main) "&path=" (enc path))} [:code nm]]
                     (when-not (now path) [:span.badge "not on main"])]
                    [:td.num n (if (= 1 n) " text" " texts")]
                    [:td.num (stamp last)]])]]])]])))

(defn- version-chips
  "Where a version was made: main, or the session (and worktree), linked to the prompt step."
  [main branch v]
  (let [[_ nn i] (re-find #"^([\w-]+)\.(\d+) " (str (:trace-subject v)))
        suf (some->> branch (re-find #"/[sa]/(.+)$") second)
        id (when (and suf nn) (str suf "@" nn))]
    (list
     (cond
       (:main? v) [:span.line.main "main"]
       suf [:span.line (str "session " (subs suf 0 (min 8 (count suf))) (when (str/includes? suf "-") " · agent"))]
       :else [:span.line "session (branch gone)"])
     (when-let [w (:worktree v)] [:span.wt {:title w} (fs/file-name w)])
     (if id
       [:a.chip {:href (str "/p/" (enc (parent-id id)) "?m=" (enc main) "#" (enc (str main "-" id "-" i)))}
        (str "prompt " (chip-text id) " · step " i)]
       (when (:main? v) [:span.chip "commit"])))))

(defn- fn-page [tips main path]
  (let [vs (fl/versions main path)
        br (fl/where main vs)
        [vs flips] (fl/unflip #(get br (:trace %)) vs)
        [file nm] (fn-split path)
        lang (ext-lang file)
        now ((fl/on-main main) path)
        word {"A" "added" "M" "changed" "D" "removed" "R" "same text reached"}]
    (page (str nm " · " main)
          [:div.app
           (topbar tips [:fns main] [main])
           [:main.wide
            [:h1.path [:code nm] [:small " in " [:a {:href (str "/fns?m=" (enc main) "&file=" (enc file))} file]]]
            [:p.sub (count vs) (if (= 1 (count vs)) " version" " versions") ", oldest first. "
             (if now "On main now." "Not on main now.")
             (when (pos? flips) (str " " flips " remove-and-add flips of older traces folded (a session that wrote in two checkouts)."))]
            (when-let [blob (if now (str/trim (git "rev-parse" (str "refs/fn/" main "/main:" path))) (some :new (reverse (remove #(= "D" (:status %)) vs))))]
              (lazy (if now "As it is on main" "Its last text") (str "/x/show?r=" (enc blob)) :kind "code" :lang lang :cls "nested"))
            [:ol.evo
             (for [[k v] (map-indexed vector vs)
                   :let [again (and (not= "D" (:status v)) (some #(= (:new v) (:new %)) (take k vs)))
                         st (if again "R" (:status v))]]
               [:li {:class (str "evo-item fnv " (if (:main? v) "on-main" "in-session") " st-" st)}
                [:div.evo-h
                 [:span.vno (str "v" (inc k))]
                 [:span.kind (word st)]
                 (version-chips main (get br (:trace v)) v)
                 [:span.ptext (clip (str/replace (str (:trace-subject v)) #"^[\w-]+\.\d+ " "") 110)]
                 [:span.t (stamp (:ct v))]]
                (case st
                  "A" (lazy "Code" (str "/x/show?r=" (:new v)) :kind "code" :lang lang :open? true)
                  "D" (lazy "Code it had" (str "/x/show?r=" (:old v)) :kind "code" :lang lang)
                  "R" (lazy "Code" (str "/x/show?r=" (:new v)) :kind "code" :lang lang)
                  (list (lazy "Diff against the version before" (str "/x/bdiff?a=" (:old v) "&b=" (:new v)) :kind "diff" :open? (not (:main? v)))
                        (lazy "Code at this version" (str "/x/show?r=" (:new v)) :kind "code" :lang lang)))
                [:div.commit "fn commit " [:code (subs (:sha v) 0 10)] " · trace commit " [:code (subs (:trace v) 0 10)]]])]]])))

;; --- http -----------------------------------------------------------------

(defn- not-recorded
  "Pathspecs of what a diff leaves out: what the recorder adds beside the project's files
  (.claude-trace/ here; reads/ and doto.md, the prompt, in a bench repo)."
  []
  (if (central?)
    ["." ":(exclude).claude-trace"]
    ["." ":(exclude)reads" ":(exclude)doto.md"]))

(defn- query [req]
  (into {} (for [kv (str/split (or (:query-string req) "") #"&")
                 :let [[k v] (str/split kv #"=" 2)] :when (seq k)]
             [k (URLDecoder/decode (or v "") "UTF-8")])))

(defn- html [s] {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body s})
(defn- text [s] {:status 200 :headers {"content-type" "text/plain; charset=utf-8"} :body (str s)})
(def ^:private bad {:status 400 :body "bad request"})
(def ^:private sha-re #"[0-9a-f]{7,40}")

(defn- handler [{:keys [uri] :as req}]
  (let [q (query req) tips (tips)]
    (try
      (cond
        (= uri "/")
        (let [ms (or (some-> (get q "m") (str/split #",")) (default-mains tips))
              newest (last (session-ids tips ms))]
          (if newest
            {:status 302 :headers {"location" (str "/p/" (enc newest) "?" (m-param ms))}}
            (html (page "trace" [:div.app (topbar tips :prompts ms) [:main.wide [:p.note "No prompts in " g/*repo* " yet. Send a prompt in a traced session and refresh."]]]))))

        (re-matches #"/p/[^/]+" uri)
        (html (prompt-page tips (URLDecoder/decode (subs uri 3) "UTF-8") (some-> (get q "m") (str/split #","))))

        (= uri "/files") (html (files-page tips (get q "m")))
        (= uri "/file") (html (file-page tips (get q "m") (get q "path")))
        (= uri "/fns") (html (fns-page tips (get q "m") (get q "file")))
        (= uri "/fn") (html (fn-page tips (get q "m") (get q "path")))

        (= uri "/x/bdiff")
        (let [a (get q "a" "") b (get q "b" "")]
          (if (and (re-matches sha-re a) (re-matches sha-re b))
            (text (str/join "\n" (remove #(re-find #"^(diff --git|index |--- |\+\+\+ )" %) (str/split-lines (git "diff" a b)))))
            bad))

        (= uri "/x/show")
        (let [r (get q "r" "")]
          (if (re-matches #"[0-9a-f]{7,40}(:[^\n]+)?" r)
            (let [{:keys [exit out]} (g/git* {} "cat-file" "-p" r)]
              (if (zero? exit)
                (text out)
                {:status 404 :headers {"content-type" "text/plain; charset=utf-8"}
                 :body (str "not in the trace repo: " r)}))
            bad))

        (= uri "/x/diff")
        (let [c (get q "c" "")]
          (if (re-matches sha-re c)
            (text (if-let [path (get q "path")]
                    (git "show" "--format=" "--patch" c "--" path)
                    (apply git "show" "--format=" "--patch" c "--" (not-recorded))))
            bad))

        (= uri "/x/net")
        (if-let [d (prompt-data tips (get q "m") (get q "p"))]
          (text (apply git "diff" (:fork d) (:end d) "--" (not-recorded)))
          bad)

        (= uri "/x/tips")
        (let [ms (or (some-> (get q "m") (str/split #",")) (default-mains tips))
              own (into (sorted-map) (filter (fn [[b _]] (some #(str/starts-with? b (str % "/")) ms)) tips))]
          {:status 200 :headers {"content-type" "application/json" "cache-control" "no-store"}
           :body (json/generate-string {:tips own :live (sort (live-in tips ms)) :latest (last (session-ids tips ms))})})

        (#{"/static/app.css" "/static/app.js" "/static/shell.js"} uri)
        {:status 200
         :headers {"content-type" (if (str/ends-with? uri ".css") "text/css" "text/javascript") "cache-control" "no-store"}
         :body (slurp (io/resource (str "trace/" (fs/file-name uri))))}

        :else {:status 404 :body "not found"})
      (catch Exception e
        (binding [*out* *err*] (println uri (ex-message e)))
        {:status 500 :headers {"content-type" "text/plain"} :body (str "error: " (ex-message e))}))))

(defn -main
  "bb trace:gui [repo] [port]: serve a trace repo (default the central one) on 127.0.0.1."
  [& [repo port]]
  (let [repo (str (fs/canonicalize (or repo g/*repo*)))
        port (parse-long (or port (if (= repo (str (fs/canonicalize g/default-repo))) "7360" "7357")))]
    (alter-var-root #'g/*repo* (constantly repo))
    (srv/run-server #'handler {:ip "127.0.0.1" :port port})
    (println (str "trace view of " repo " on http://127.0.0.1:" port "/"))
    @(promise)))
