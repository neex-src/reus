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
    /p/NN?m=a,b          prompt NN of a,b
    /files?m=a           every file a edited
    /file?m=a&path=P     every edit of P, in order: the file's evolution
    /x/show?r=REV        git cat-file -p REV (a blob sha, or <commit>:<path>)
    /x/diff?c=SHA[&path=P]   that commit's change outside reads/ and doto.md
    /x/net?m=a&p=NN      the net change of a prompt
    /x/tips              branch tips + running prompts, for the live poll"
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [org.httpkit.server :as srv]
            [trace.git :as g :refer [git]]
            [trace.agents :as agents]
            [trace.read :refer [tips mains prompt-nos prompt-data all-prompts live-branches ext-lang central?]])
  (:import [java.net URLDecoder URLEncoder]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(defn- newest-prompt
  "The prompt branch with the newest tip commit."
  [tips]
  (let [ours (set (filter #(re-find #"-p\d+$" %) (keys tips)))]
    (->> (str/split-lines (git "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)" "refs/heads"))
         (filter ours) first)))

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
(defn- first-line [s] (first (str/split-lines (str s))))

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

(defn- step-view [main nn t0 s session]
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
        (when (:lines s) [:span.lines (str (:lines s) " lines")])
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
                  (lazy (str "Output · " (:lines s) " lines")
                        (str "/x/show?r=" (enc (str (:sha s) ":" (:result s)))) :kind "text"))
                (list
                (lazy (str "Diff · " (str/join ", " (map :path (:files s))))
                      (str "/x/diff?c=" (:sha s)) :kind "diff" :open? true)
                [:div.after
                 (for [f (:files s)]
                   (lazy (str "File after this edit · " (:path f))
                         (str "/x/show?r=" (enc (str (:sha s) ":" (:path f))))
                         :kind "code" :lang (ext-lang (:path f)) :cls "nested"))]))
         :read (list
                (if (:result s)
                  (lazy (str "Output · " (:lines s) " lines")
                        (str "/x/show?r=" (enc (str (:sha s) ":" (:result s)))) :kind "text" :open? true)
                  [:div.note "Same output as the last time this file was read, so nothing new was recorded."])
                (when (:blob s)
                 (lazy [:span "File as it was when read · " [:span.file (:read-file s)] " · blob " [:code.sha (subs (:blob s) 0 10)]]
                      (str "/x/show?r=" (:blob s)) :kind "code" :lang (ext-lang (:read-file s)) :cls "nested blob")))
         :run (if (:result s)
                (lazy (str "Output · " (:lines s) " lines")
                      (str "/x/show?r=" (enc (str (:sha s) ":" (:result s)))) :kind "text" :open? true)
                [:div.note "No output recorded."])
         nil)
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
       [:details [:summary "result · " (count (str/split-lines result)) " lines"] (code-block result "plaintext")])]))

(defn- notes-of
  "The session's notifications between this prompt's start and the next prompt of the same session."
  [tips d]
  (when-let [session (:session d)]
    (let [end (->> (all-prompts tips (:main d))
                   (filter #(and (= session (:session %)) (> (:t0 %) (:t0 d))))
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
    (some-> (newest-prompt tips) (str/replace #"-p\d+$" "") vector)
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
      [:a {:href (str "/files?m=" (enc m)) :class (when (= current m) "on")} (str "Files · " m)])]
   [:div.live
    [:span#livestate.state "watching"]
    [:label {:title "Show each command of a shell step with the output it printed. A guess: shell output has no separators, so commands are told apart by echo markers and by the shape of their lines."}
     [:input#splitout {:type "checkbox"}] " output per command (experimental)"]
    [:label [:input#follow {:type "checkbox"}] " follow live"]]])

(defn- prompt-index [tips ms current]
  (let [live (live-branches)
        nos (sort-by parse-long (distinct (mapcat #(prompt-nos tips %) ms)))]
    [:aside.index
     [:div.legend (for [[k t] kinds] [:span {:class (str "lg k-" (name k))} [:i] t])]
     [:ol
      (for [nn (reverse nos)
            :let [ds (keep #(prompt-data tips % nn) ms)
                  text (some :prompt ds)]]
        [:li
         [:a {:class (str "pr" (when (= nn current) " cur")) :href (str "/p/" nn "?" (m-param ms))}
          [:span.nn nn]
          [:span.ptext (clip text 90)
           (when (central?) [:span.when (some-> (some :t0 ds) stamp)])]
          [:span.tapes
           (for [d ds :let [b (str (:main d) "-p" nn)]]
             [:span.trow
              [:span.mname (:main d) (when (live b) [:i.pulse])]
              (tape (take 64 (:steps d)) :cls "mini")
              [:span.cnt (count (:steps d)) (when (> (count (:steps d)) 64) [:em "+"])]])]]])]]))

(defn- prompt-page [tips nn ms]
  (let [all (mains tips)
        ms (or (seq (filter (set all) ms)) (default-mains tips) all)
        central (central?)
        ds (keep #(prompt-data tips % nn) ms)
        text (some :prompt ds)
        live (live-branches)]
    (page (str "Prompt " nn " · trace")
          [:div.app
           (topbar tips :prompts ms)
           [:div.shell
            (prompt-index tips ms nn)
            [:main
             [:section.head
              [:div.pno [:span "Prompt"] [:b nn]]
              [:div.md.prompt-text text]
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
                    (when (live (str (:main d) "-p" nn)) [:span.running [:i.pulse] "running"])]])]]
              [:div.tools
               [:div.filters "Show "
                (for [[k t] kinds] [:label {:class (str "f k-" (name k))} [:input {:type "checkbox" :checked true :data-filter (name k)}] t])]
               [:div.btns
                [:button {:type "button" :data-act "open"} "Expand all"]
                [:button {:type "button" :data-act "close"} "Collapse all"]
                (when (and (not central) (> (count all) 1))
                  (for [m all] [:a.solo {:href (str "/p/" nn "?m=" (enc m))} (str "Only " m)]))
                (when (and (not central) (< (count ds) (count all))) [:a.solo {:href (str "/p/" nn)} "All side by side"])]]]
             [:div.cols {:style (str "--n:" (max 1 (count ds)))}
              (for [d ds :let [{:keys [main steps t0]} d]]
                [:section.col
                 [:h2 main [:small (count steps) " steps · " (mmss (- (:t1 d) t0))]]
                 (tape steps :href-fn #(str "#" main "-" nn "-" (:i %)) :cls "big")
                 (lazy "Net change of this prompt" (str "/x/net?m=" (enc main) "&p=" nn) :kind "diff" :cls "net")
                 [:ol.steps (for [s steps] (step-view main nn t0 s (:session d)))]
                 (when-let [ns (seq (notes-of tips d))]
                   [:div.events
                    [:div.reply-h (str "Background events · " (count ns))]
                    [:ol (for [n ns] (note-view t0 n (:session d)))]])
                 [:div.reply
                  [:div.reply-h "Reply"]
                  (if (:reply d)
                    [:div.md (:reply d)]
                    [:div.note (if (live (str main "-p" nn)) "Still working…" "No reply recorded.")])]])]]]]
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
                 [:td (for [nn (:prompts r)] [:a.chip {:href (str "/p/" nn "?m=" (enc main))} nn])]])]]]])))

(defn- file-page [tips main path]
  (let [ds (all-prompts tips main)
        edits (for [d ds s (:steps d) :when (some #(= path (:path %)) (:files s))] [d s])
        others (remove #{main} (mains tips))]
    (page (str path " · " main)
          [:div.app
           (topbar tips main [main])
           [:main.wide
            [:h1.path path]
            [:p.sub (count edits) " edits by " main ". "
             (for [o others] [:a {:href (str "/file?m=" (enc o) "&path=" (enc path))} (str "Same file in " o)])]
            (lazy (str "The file now, at " main "'s tip")
                  (str "/x/show?r=" (enc (str (get tips main) ":" path))) :kind "code" :lang (ext-lang path) :cls "nested")
            [:ol.evo
             (for [[d s] edits]
               [:li {:class (str "evo-item " (kind-class s))}
                [:div.evo-h
                 [:a.chip {:href (str "/p/" (:nn d) "?m=" (enc main) "#" main "-" (:nn d) "-" (:i s))} (str "prompt " (:nn d) " · step " (:i s))]
                 [:span.tool (:tool (:call s))]
                 [:span.ptext (clip (:prompt d) 110)]
                 [:span.delta [:b.add (str "+" (:add s))] " " [:b.del (str "−" (:del s))]]]
                (lazy "Diff" (str "/x/diff?c=" (:sha s) "&path=" (enc path)) :kind "diff" :open? true)])]]])))

;; --- http -----------------------------------------------------------------

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
              newest (newest-prompt (select-keys tips (filter (fn [b] (some #(str/starts-with? b (str % "-p")) ms)) (keys tips))))]
          (if newest
            {:status 302 :headers {"location" (str "/p/" (second (re-find #"-p(\d+)$" newest)) "?" (m-param ms))}}
            (html (page "trace" [:div.app (topbar tips :prompts ms) [:main.wide [:p.note "No prompts in " g/*repo* " yet. Send a prompt in a traced session and refresh."]]]))))

        (re-matches #"/p/\d+" uri)
        (html (prompt-page tips (subs uri 3) (some-> (get q "m") (str/split #","))))

        (= uri "/files") (html (files-page tips (get q "m")))
        (= uri "/file") (html (file-page tips (get q "m") (get q "path")))

        (= uri "/x/show")
        (let [r (get q "r" "")]
          (if (re-matches #"[0-9a-f]{7,40}(:[^\n]+)?" r) (text (git "cat-file" "-p" r)) bad))

        (= uri "/x/diff")
        (let [c (get q "c" "")]
          (if (re-matches sha-re c)
            (text (if-let [path (get q "path")]
                    (git "show" "--format=" "--patch" c "--" path)
                    (git "show" "--format=" "--patch" c "--" "." ":(exclude)reads" ":(exclude)doto.md")))
            bad))

        (= uri "/x/net")
        (if-let [d (prompt-data tips (get q "m") (get q "p"))]
          (text (git "diff" (:fork d) (:tip d) "--" "." ":(exclude)reads" ":(exclude)doto.md"))
          bad)

        (= uri "/x/tips")
        {:status 200 :headers {"content-type" "application/json"}
         :body (json/generate-string {:tips tips :live (sort (live-branches))})}

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
