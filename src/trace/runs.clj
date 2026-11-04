(ns trace.runs
  "The viewer's pages for runs of the SAME task: one agent (Copilot, Claude, anything reus records) given the
  same prompts in the same starting folder under different settings (model, reasoning effort), side by side.

    /runs                 the tasks: every project named <task>--<arm>, with its arms
    /runs/<task>          per arm the summary (does the end state pass its check, calls, failures, lines, time,
                          tokens, cost), then prompt by prompt: what each arm did, call by call, with the diff
                          of each edit and the output of each command behind a click

  No configuration: a run is a project (a trace.record project is named after its directory) called
  `<task>--<arm>`, and the arm's session is the project's session with the most prompts. Everything on the pages
  is read from the trace repo (trace.read); nothing here is on the recording path.

  The end state check: the tree of the session's last commit is written to .git/runs-check/<sha>/ and the
  command in its `.reus-check` (e.g. `node --test`) runs there, once per commit (the result is kept next to it).
  That is the agent's own output being run, as the agent ran it; a tree without `.reus-check` has no check.

  Cost: tokens at list price (`prices`, $ per million tokens, cache write 1.25x input, read 0.1x); a model
  without a price (a Copilot model billed by premium requests) shows tokens only. The input tokens of a Copilot
  session are Copilot's own count minus the cached part (trace.copilot), so they compare with Claude's."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [trace.git :as g :refer [git]]
            [trace.read :as r])
  (:import [java.net URLEncoder]))

;; --- numbers --------------------------------------------------------------------------------

(def prices
  "model -> $ per million tokens {:in :out :cr (cache read) :cw (cache write)}; list prices (claude-api skill, 2026-10-06)."
  {"claude-haiku-5-5" {:in 0.10 :out 0.50 :cr 0.01 :cw 0.125}
   "claude-haiku-4-5" {:in 1.00 :out 5.00 :cr 0.10 :cw 1.25}
   "claude-sonnet-5-5" {:in 2.00 :out 10.00 :cr 0.20 :cw 2.50}
   "claude-sonnet-5" {:in 2.00 :out 10.00 :cr 0.20 :cw 2.50}
   "claude-sonnet-4-6" {:in 3.00 :out 15.00 :cr 0.30 :cw 3.75}
   "claude-opus-5-5" {:in 4.00 :out 20.00 :cr 0.40 :cw 5.00}
   "claude-opus-5" {:in 5.00 :out 25.00 :cr 0.50 :cw 6.25}
   "claude-opus-4-8" {:in 5.00 :out 25.00 :cr 0.50 :cw 6.25}})

(defn cost
  "USD of a turn {:model :usage}, nil when the model has no price."
  [{:keys [model usage]}]
  (when-let [pr (get prices model)]
    (/ (+ (* (:in pr) (or (:input_tokens usage) 0)) (* (:out pr) (or (:output_tokens usage) 0))
          (* (:cr pr) (or (:cache_read_input_tokens usage) 0)) (* (:cw pr) (or (:cache_creation_input_tokens usage) 0)))
       1e6)))

(defn- usd [x] (if x (if (< x 0.01) (format "$%.4f" x) (format "$%.3f" x)) "–"))
(defn- thou [n] (if n (let [s (str n)] (str/replace s #"\B(?=(\d{3})+(?!\d))" ",")) "–"))
(defn- secs [s] (let [s (max 0 (long s))] (if (>= s 60) (format "%d:%02d" (quot s 60) (mod s 60)) (str s "s"))))
(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))
(defn- first-line [s] (first (str/split-lines (str s))))

;; --- the runs --------------------------------------------------------------------------------

(defn tasks
  "{task {arm project}} of the projects named <task>--<arm>."
  [tips]
  (reduce (fn [m p] (if-let [[_ t a] (re-matches #"(.+?)--(.+)" p)] (assoc-in m [t a] p) m))
          (sorted-map) (r/mains tips)))

(defn- session-of
  "The session of a project with the most prompts (the newest on a tie): its prompt ids."
  [tips project]
  (let [ids (vec (remove #(str/includes? (first (str/split % #"@")) "-") ; an agent run is not the arm's session
                         (r/prompt-nos tips project)))] ; oldest first
    (->> ids
         (group-by #(first (str/split % #"@")))
         vals
         (sort-by (juxt count #(.indexOf ^java.util.List ids (last %))))
         last)))

(defn- step-failed? [s] (boolean (:err s)))

(defn- sum-usage [turns]
  (apply merge-with + {} (map #(select-keys (:usage %) [:input_tokens :output_tokens :cache_read_input_tokens :cache_creation_input_tokens :reasoning_tokens]) turns)))

(defn prompt-metrics
  "Numbers of one prompt's data (trace.read/prompt-data): calls by kind, failed calls, lines, seconds, the turn."
  [d]
  (let [steps (:steps d) by (frequencies (map :kind steps))]
    {:calls (count steps) :edits (get by :edit 0) :runs (get by :run 0) :reads (get by :read 0)
     :failed (count (filter step-failed? steps))
     :add (reduce + 0 (map :add steps)) :del (reduce + 0 (map :del steps))
     :secs (when (and (:t0 d) (:t1 d)) (- (:t1 d) (:t0 d)))
     :turn (:turn-end d) :cost (cost (:turn-end d))}))

(defn arm-data
  "{:arm :project :ids :prompts [{:d prompt-data :m metrics}] :total metrics-sum :tip :session}, nil for a project with no session."
  [tips arm project]
  (when-let [ids (seq (session-of tips project))]
    (let [ps (mapv (fn [id] (let [d (r/prompt-data tips project id)] {:id id :d d :m (prompt-metrics d)})) ids)
          ms (map :m ps)
          turns (keep :turn ms)
          costs (keep :cost ms)]
      {:arm arm :project project :ids ids :prompts ps
       :tip (:tip (:d (peek ps))) :session (:session (:d (first ps)))
       :total {:calls (reduce + (map :calls ms)) :edits (reduce + (map :edits ms)) :failed (reduce + (map :failed ms))
               :add (reduce + (map :add ms)) :del (reduce + (map :del ms))
               :secs (reduce + (keep :secs ms)) :usage (sum-usage turns)
               :cost (when (seq costs) (reduce + costs))}})))

;; --- the end state check ---------------------------------------------------------------------

(defn- check-dir [] (str (fs/path g/*repo* ".git" "runs-check")))

(defn- tap-count [out k]
  (some->> (re-find (re-pattern (str "(?m)^(?:ℹ|#) " k " (\\d+)")) out) second parse-long))

(defn check
  "{:cmd :exit :pass :fail :out} of the check of the tree at commit sha, run once (kept in .git/runs-check/<sha>.edn),
  nil when the tree has no .reus-check."
  [sha]
  (let [{:keys [exit out]} (g/git* {} "show" (str sha ":.reus-check"))
        cmd (when (zero? exit) (str/trim out))]
    (when-not (str/blank? cmd)
      (let [keep-f (fs/path (check-dir) (str sha ".edn"))]
        (if (fs/exists? keep-f)
          (edn/read-string (slurp (str keep-f)))
          (let [dir (fs/path (check-dir) sha)]
            (fs/delete-tree dir)
            (fs/create-dirs dir)
            (p/shell {:out :string :err :string :continue true}
                     "sh" "-c" (str "git -C " (pr-str (str g/*repo*)) " archive " sha " | tar -x -C " (pr-str (str dir))))
            (fs/delete-tree (fs/path dir ".claude-trace"))
            (let [res (p/shell {:dir (str dir) :out :string :err :out :continue true}
                               "timeout" "120" "sh" "-c" cmd)
                  o (str (:out res))
                  r {:cmd cmd :exit (:exit res) :pass (tap-count o "pass") :fail (tap-count o "fail")
                     :out (if (> (count o) 6000) (subs o (- (count o) 6000)) o)}]
              (spit (str keep-f) (pr-str r))
              r)))))))

;; --- pages -----------------------------------------------------------------------------------

(def ^:private css "
.runs-wrap { max-width: 1500px; margin: 0 auto; padding: 18px 16px 60px; }
.runs-wrap h1 { font-size: 22px; margin: 4px 0 10px; }
.runs-wrap h2 { font-size: 16px; margin: 26px 0 8px; }
.runs-wrap .crumb { color: var(--muted); font-size: 13px; }
.runs-wrap .crumb a { text-decoration: none; }
table.arms { border-collapse: collapse; font-size: 14px; background: var(--panel); width: 100%; margin: 8px 0 14px; }
table.arms th, table.arms td { padding: 7px 10px; border-bottom: 1px solid var(--line); text-align: right; white-space: nowrap; }
table.arms th:first-child, table.arms td:first-child { text-align: left; }
table.arms th { font-size: 12px; color: var(--muted); font-weight: 600; }
.pass { color: #2c7a3f; font-weight: 600; } .failx { color: #c0392b; font-weight: 600; }
@media (prefers-color-scheme: dark) { :root:not([data-theme=light]) .pass { color: #7fd08f; } :root:not([data-theme=light]) .failx { color: #ee6b5b; } }
.ptext { background: var(--sunk); padding: 8px 12px; border-radius: 6px; margin: 6px 0 10px; }
.cols { display: grid; grid-template-columns: repeat(var(--n), minmax(0, 1fr)); gap: 12px; align-items: start; }
.col { border: 1px solid var(--line); border-radius: 6px; padding: 8px 10px; background: var(--panel); min-width: 0; }
.col h3 { margin: 0 0 4px; font-size: 14px; }
.col .nums { color: var(--muted); font-size: 12px; margin-bottom: 6px; }
ol.calls { list-style: none; margin: 4px 0; padding: 0; font-size: 13px; }
ol.calls > li, ol.calls details { margin: 2px 0; }
ol.calls summary, ol.calls li.flat { font-family: var(--mono); font-size: 12px; overflow-wrap: anywhere; cursor: pointer; }
ol.calls li.flat { cursor: default; padding-left: 14px; }
.chip { display: inline-block; min-width: 38px; text-align: center; font-size: 10px; padding: 0 5px; border-radius: 8px; margin-right: 6px; background: var(--sunk); color: var(--muted); }
.chip.edit { background: #dff3e3; color: #1d5a2c; } .chip.run { background: #e3ecfa; color: #1f4a8a; } .chip.err { background: #fbe0dc; color: #8e2a1d; }
.add { color: #2c7a3f; } .del { color: #c0392b; }
.reply { margin-top: 6px; font-size: 13px; }
.reply pre { white-space: pre-wrap; margin: 4px 0; }
pre.checkout { white-space: pre-wrap; font-size: 12px; max-height: 320px; overflow: auto; }
@media (max-width: 900px) { .cols { grid-template-columns: 1fr; } }
")

(defn- page [title body]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title]
          [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;600&display=swap"}]
          [:link {:rel "stylesheet" :href "/static/app.css"}]
          [:style (h/raw css)]
          [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]]
         [:body {:data-watch "{}"}
          [:header.top
           [:a.brand {:href "/"} "reus trace"]
           [:nav [:a {:href "/"} "Prompts"] [:a {:href "/mirrors"} "Mirrors"] [:a.on {:href "/runs"} "Runs"]]]
          body
          [:script {:src "/static/shell.js"}]
          [:script {:src "/static/app.js"}]]])))

(defn- lazy [title src kind]
  [:details {:class "lazy" :data-src src :data-kind kind} [:summary title] [:div.lazy-body [:div.loading "loading…"]]])

(defn- step-label [{:keys [call cmd kind files subject]}]
  (let [{:keys [tool input code]} call
        target (or (get input "file_path") (get input "path"))]
    (cond code (clip (first-line code) 160)
          target (str tool " " target)
          (seq files) (str/join ", " (map :path files))
          :else (clip (or cmd subject) 120))))

(defn- step-li [s]
  (let [cls (cond (:err s) "err" (= :edit (:kind s)) "edit" (= :run (:kind s)) "run" :else "")
        head (list [:span {:class (str "chip " cls)} (cond (:err s) "failed" :else (name (:kind s)))]
                   (clip (step-label s) 150)
                   (when (or (pos? (:add s 0)) (pos? (:del s 0))) (list " " [:span.add (str "+" (:add s))] " " [:span.del (str "−" (:del s))]))
                   (when-let [ms (get-in s [:trailers "Duration-Ms"])] [:small {:style "color:var(--muted)"} (str "  " ms " ms")]))]
    [:li
     (cond (seq (:files s)) [:details {:class "lazy" :data-src (str "/x/diff?c=" (:sha s)) :data-kind "diff"} [:summary head] [:div.lazy-body [:div.loading "loading…"]]]
           (:result s) [:details {:class "lazy" :data-src (str "/x/show?r=" (:result s)) :data-kind "text"} [:summary head] [:div.lazy-body [:div.loading "loading…"]]]
           :else [:div.flat.flat head])]))

(defn- nums-line [m]
  (str (:calls m) " calls · " (:edits m) " edits · " (:failed m) " failed · +" (:add m) " −" (:del m)
       (when (:secs m) (str " · " (secs (:secs m))))
       (when-let [u (:usage (:turn m))]
         (str " · out " (thou (:output_tokens u)) (when (:reasoning_tokens u) (str " (" (thou (:reasoning_tokens u)) " thinking)"))
              " · cache " (thou (:cache_read_input_tokens u))))
       (when (:cost m) (str " · " (usd (:cost m))))))

(defn- summary-table [arms]
  [:table.arms
   [:thead [:tr [:th "arm"] [:th "end state"] [:th "calls"] [:th "edits"] [:th "failed"] [:th "lines"] [:th "time"]
            [:th "input"] [:th "output"] [:th "thinking"] [:th "cache read"] [:th "cost"]]]
   [:tbody
    (for [{:keys [arm total check] :as a} arms
          :let [u (:usage total)]]
      [:tr
       [:td [:b arm]]
       [:td (cond (nil? check) "–"
                  (zero? (:exit check)) [:span.pass (str "✓ " (or (:pass check) "") " pass")]
                  :else [:span.failx (str "✗ " (or (:fail check) "?") " fail")])]
       [:td (:calls total)] [:td (:edits total)] [:td (:failed total)]
       [:td [:span.add (str "+" (:add total))] " " [:span.del (str "−" (:del total))]]
       [:td (secs (:secs total))]
       [:td (thou (:input_tokens u))] [:td (thou (:output_tokens u))] [:td (thou (:reasoning_tokens u))]
       [:td (thou (:cache_read_input_tokens u))]
       [:td (usd (:cost total))]])]])

(defn- task-page [tips task]
  (let [arms (->> (get (tasks tips) task)
                  (keep (fn [[arm project]] (when-let [a (arm-data tips arm project)]
                                              (assoc a :check (check (:tip a))))))
                  (sort-by :arm) vec)
        n (apply max 0 (map #(count (:prompts %)) arms))]
    (page (str task " · runs")
          [:main.runs-wrap
           [:div.crumb [:a {:href "/runs"} "Runs"] " / " task]
           [:h1 task]
           (if (empty? arms)
             [:p "No run of this task is recorded."]
             (list
              (summary-table arms)
              [:p.crumb "Cost is list price of the tokens (input counts what was not cached). The check is the command in the final tree's "
               [:code ".reus-check"] ", run on that tree."]
              (for [a arms :when (:check a)]
                [:details [:summary (str (:arm a) ": " (:cmd (:check a)) " → exit " (:exit (:check a)))]
                 [:pre.checkout (:out (:check a))]])
              (for [i (range n)
                    :let [pd (some #(:d (nth (:prompts %) i nil)) arms)]]
                [:section
                 [:h2 (str "Prompt " (inc i))]
                 [:div.ptext (:prompt pd)]
                 [:div.cols {:style (str "--n:" (count arms))}
                  (for [a arms :let [p (nth (:prompts a) i nil)]]
                    [:div.col
                     [:h3 (:arm a)
                      (when p [:small " " [:a {:href (str "/p/" (enc (:id p)) "?m=" (enc (:project a)))} "open in trace"]])]
                     (if-not p
                       [:div.nums "not reached"]
                       (let [{:keys [d m]} p]
                         (list
                          [:div.nums (nums-line m)]
                          [:ol.calls (for [s (:steps d)] (step-li s))]
                          (lazy "Net change of this prompt" (str "/x/net?m=" (enc (:project a)) "&p=" (enc (:id p))) "diff")
                          [:details.reply [:summary "Reply"] [:pre (:reply d)]])))])]])))])))

(defn- index-page [tips]
  (page "Runs"
        [:main.runs-wrap
         [:h1 "Runs of the same task"]
         [:p.crumb "Projects named " [:code "<task>--<arm>"] " are the arms of one task."]
         (if-let [ts (seq (tasks tips))]
           [:table.arms
            [:thead [:tr [:th "task"] [:th "arms"]]]
            [:tbody (for [[t arms] ts]
                      [:tr [:td [:a {:href (str "/runs/" (enc t))} t]] [:td (str/join ", " (keys arms))]])]]
           [:p "None yet."])]))

(defn route
  "The response for /runs or /runs/<task>, nil when the uri is not ours."
  [uri]
  (let [html (fn [s] {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body s})
        tips (r/tips)]
    (cond (= uri "/runs") (html (index-page tips))
          (re-matches #"/runs/[^/]+" uri)
          (let [t (java.net.URLDecoder/decode (subs uri 6) "UTF-8")]
            (if (get (tasks tips) t) (html (task-page tips t)) {:status 404 :body "no such task"})))))
