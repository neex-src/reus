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

(defn sessions-of
  "The sessions of a project, oldest first: [{:sid8 :ids}] (an agent run is not a session of the arm)."
  [tips project]
  (let [ids (vec (remove #(str/includes? (first (str/split % #"@")) "-") (r/prompt-nos tips project)))] ; oldest first
    (->> ids
         (group-by #(first (str/split % #"@")))
         (map (fn [[sid8 is]] {:sid8 sid8 :ids (vec is)}))
         (sort-by #(.indexOf ^java.util.List ids (first (:ids %))))
         vec)))

(defn- step-failed? [s] (boolean (:err s)))

(defn step-kind
  "What a call was, by the tool that made it (trace.read calls any call that names one file a read): :edit when it
  changed files or is a writing tool, :read for the reading tools, :run for a shell call, else trace.read's guess."
  [s]
  (let [tool (get-in s [:trailers "Tool"])]
    (cond (seq (:files s)) :edit
          (#{"Write" "Edit" "MultiEdit" "NotebookEdit"} tool) :edit
          (#{"Read" "Glob" "Grep" "WebFetch" "WebSearch"} tool) :read
          (= "Bash" tool) :run
          :else (:kind s))))

(defn- sum-usage [turns]
  (apply merge-with + {} (map #(select-keys (:usage %) [:input_tokens :output_tokens :cache_read_input_tokens :cache_creation_input_tokens :reasoning_tokens :copilot_cost]) turns)))

(defn prompt-metrics
  "Numbers of one prompt's data (trace.read/prompt-data): calls by kind, failed calls, lines, seconds, the turn."
  [d]
  (let [steps (:steps d) by (frequencies (map step-kind steps))]
    {:calls (count steps) :edits (get by :edit 0) :runs (get by :run 0) :reads (get by :read 0)
     :failed (count (filter step-failed? steps))
     :add (reduce + 0 (map :add steps)) :del (reduce + 0 (map :del steps))
     :secs (when (and (:t0 d) (:t1 d)) (- (:t1 d) (:t0 d)))
     :turn (:turn-end d) :cost (cost (:turn-end d))}))

(defn arm-data
  "One run of an arm: {:arm :project :sid8 :at :ids :prompts [{:d prompt-data :m metrics}] :total metrics-sum :tip :session}
  for the session sid8 of the project (default: its newest), nil when it has none."
  [tips arm project & [sid8]]
  (when-let [{:keys [ids]} (let [ss (sessions-of tips project)] (if sid8 (first (filter #(= sid8 (:sid8 %)) ss)) (peek ss)))]
    (let [ps (mapv (fn [id] (let [d (r/prompt-data tips project id)] {:id id :d d :m (prompt-metrics d)})) ids)
          ms (map :m ps)
          turns (keep :turn ms)
          costs (keep :cost ms)]
      {:arm arm :project project :ids ids :sid8 (first (str/split (first ids) #"@")) :at (:t0 (:d (first ps))) :prompts ps
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
            (let [res (apply p/shell {:dir (str dir) :out :string :err :out :continue true}
                               (concat (when (fs/which "timeout") ["timeout" "120"]) ["sh" "-c" cmd]))
                  o (str (:out res))
                  r {:cmd cmd :exit (:exit res) :pass (tap-count o "pass") :fail (tap-count o "fail")
                     :out (if (> (count o) 6000) (subs o (- (count o) 6000)) o)}]
              (spit (str keep-f) (pr-str r))
              r)))))))

;; --- pages -----------------------------------------------------------------------------------

(def ^:private css "
.runs-wrap { max-width: 1500px; margin: 0 auto; padding: 18px 16px 60px; }
.runs-wrap h1 { font-size: 22px; margin: 4px 0 10px; }
.runs-wrap h2 { font-size: 17px; margin: 30px 0 8px; padding-top: 10px; border-top: 2px solid var(--line); }
.runs-wrap h4 { font-size: 12px; margin: 14px 0 4px; color: var(--muted); text-transform: uppercase; letter-spacing: .04em; }
.runs-wrap .crumb { color: var(--muted); font-size: 13px; }
.runs-wrap .crumb a { text-decoration: none; }
.toolbar { margin: 6px 0 4px; display: flex; gap: 8px; }
.toolbar button { font: inherit; font-size: 13px; padding: 4px 12px; border: 1px solid var(--line); border-radius: 6px; background: var(--panel); color: inherit; cursor: pointer; }
.toolbar button:hover { background: var(--sunk); }
table.arms { border-collapse: collapse; font-size: 14px; background: var(--panel); width: 100%; margin: 6px 0 12px; }
table.arms th, table.arms td { padding: 7px 10px; border-bottom: 1px solid var(--line); text-align: right; white-space: nowrap; }
table.arms th:first-child, table.arms td:first-child { text-align: left; }
table.arms th { font-size: 12px; color: var(--muted); font-weight: 600; }
table.arms tr.older td { color: var(--muted); }
table.delta td, table.delta th { font-variant-numeric: tabular-nums; white-space: normal; padding: 7px 6px; }
table.delta td small { font-size: 11px; }
.pass { color: #2c7a3f; font-weight: 600; } .failx { color: #c0392b; font-weight: 600; }
.good { color: #2c7a3f; } .bad { color: #c0392b; } .flat { color: var(--muted); }
@media (prefers-color-scheme: dark) { :root:not([data-theme=light]) .pass, :root:not([data-theme=light]) .good { color: #7fd08f; } :root:not([data-theme=light]) .failx, :root:not([data-theme=light]) .bad { color: #ee6b5b; } }
.ptext { background: var(--sunk); padding: 8px 12px; border-radius: 6px; margin: 6px 0 10px; white-space: pre-wrap; }
.cols { display: grid; grid-template-columns: repeat(var(--n), minmax(0, 1fr)); gap: 12px; align-items: start; }
.col { border: 1px solid var(--line); border-radius: 6px; padding: 8px 10px; background: var(--panel); min-width: 0; }
.col h3 { margin: 0 0 6px; font-size: 15px; }
.col h3 small { font-weight: 400; color: var(--muted); margin-left: 6px; }
.strip { display: flex; flex-wrap: wrap; gap: 8px 8px; margin: 0 0 8px; padding: 6px 8px; background: var(--sunk); border-radius: 6px; }
.grp { display: flex; flex-direction: column; gap: 2px; }
.grp > .gl { font-size: 10px; text-transform: uppercase; letter-spacing: .05em; color: var(--muted); border-bottom: 1px solid var(--line); }
.grp > .gi { display: flex; gap: 4px; }
.st { width: 58px; flex: none; }
.st .sl { font-size: 10px; color: var(--muted); }
.st .sv { font-size: 15px; font-weight: 600; font-variant-numeric: tabular-nums; line-height: 1.2; }
.st .sd { font-size: 10px; font-variant-numeric: tabular-nums; min-height: 13px; line-height: 1.15; }
ol.calls { list-style: none; margin: 4px 0; padding: 0; font-size: 13px; }
ol.calls > li, ol.calls details { margin: 2px 0; }
ol.calls summary, ol.calls .plain { font-family: var(--mono); font-size: 12px; overflow-wrap: anywhere; cursor: pointer; }
ol.calls .plain { cursor: default; padding-left: 14px; }
.chip { display: inline-block; min-width: 38px; text-align: center; font-size: 10px; padding: 0 5px; border-radius: 8px; margin-right: 6px; background: var(--sunk); color: var(--muted); }
.chip.edit { background: #dff3e3; color: #1d5a2c; } .chip.run { background: #e3ecfa; color: #1f4a8a; } .chip.err { background: #fbe0dc; color: #8e2a1d; }
.add { color: #2c7a3f; } .del { color: #c0392b; }
.rr { margin-top: 6px; font-size: 13px; }
.rr pre { white-space: pre-wrap; margin: 4px 0; font-family: inherit; }
pre.checkout, pre.body { white-space: pre-wrap; overflow-wrap: anywhere; font: 12px/1.4 var(--mono); background: var(--sunk); padding: 6px 8px; border-radius: 4px; margin: 4px 0; }
pre.body .a { color: #2c7a3f; } pre.body .d { color: #c0392b; } pre.body .h { color: var(--muted); }
table.files { font-size: 12px; font-family: var(--mono); border-collapse: collapse; margin: 4px 0 8px; }
table.files td { padding: 1px 10px 1px 0; }
a.toggle { font-size: 12px; margin-left: 6px; }
@media (max-width: 900px) { .cols { grid-template-columns: 1fr; } }
")

(def ^:private static-css "
:root { --panel: #fff; --sunk: #f3f4f6; --line: #dcdfe4; --muted: #6b7280; --bg: #fff; --fg: #1c1e21; --mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
@media (prefers-color-scheme: dark) { :root { --panel: #1b1e23; --sunk: #23272e; --line: #333842; --muted: #9aa3af; --bg: #14161a; --fg: #e6e8eb; } }
body { margin: 0; background: var(--bg); color: var(--fg); font: 15px/1.45 system-ui, -apple-system, 'Segoe UI', sans-serif; }
code { font-family: var(--mono); font-size: 0.92em; }
summary { cursor: pointer; }
")

(def ^:private toggle-script
  "function reusAll(open){document.querySelectorAll('main.runs-wrap details').forEach(function(d){d.open=open})}")

(defn- page [title body & {:keys [static?]}]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title]
          (if static?
            [:style (h/raw (str static-css css))]
            (list
             [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Sans:wght@400;500;600;700&family=JetBrains+Mono:wght@400;600&display=swap"}]
             [:link {:rel "stylesheet" :href "/static/app.css"}]
             [:style (h/raw css)]
             [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"}]))]
         (if static?
           [:body body [:script (h/raw toggle-script)]]
           [:body {:data-watch "{}"}
            [:header.top
             [:a.brand {:href "/"} "reus trace"]
             [:nav [:a {:href "/"} "Prompts"] [:a {:href "/mirrors"} "Mirrors"] [:a.on {:href "/runs"} "Runs"]]]
            body
            [:script (h/raw toggle-script)]
            [:script {:src "/static/shell.js"}]
            [:script {:src "/static/app.js"}]])])))

(defn- lazy [title src kind]
  [:details {:class "lazy" :data-src src :data-kind kind} [:summary title] [:div.lazy-body [:div.loading "loading…"]]])

(defn- clipped [s n] (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) "\n… " (- (count s) n) " more characters") s)))

(defn- diff-pre [text n]
  [:pre.body (for [l (str/split-lines (clipped text n))]
               [:span {:class (cond (str/starts-with? l "+++") "h" (str/starts-with? l "---") "h" (str/starts-with? l "@@") "h"
                                    (str/starts-with? l "diff ") "h" (str/starts-with? l "index ") "h"
                                    (str/starts-with? l "+") "a" (str/starts-with? l "-") "d" :else "")}
                (str l "\n")])])

(def ^:private not-trace [":(exclude).claude-trace"])

(defn- step-label [{:keys [call cmd kind files subject]}]
  (let [{:keys [tool input code]} call
        target (or (get input "file_path") (get input "path"))]
    (cond code (clip (first-line code) 160)
          target (str tool " " target)
          (seq files) (str/join ", " (map :path files))
          :else (clip (or cmd subject) 120))))

(defn- exit-label [err] (if-let [[_ n] (re-find #"Exit code (\d+)" (str err))] (str "exit " n) "failed"))

(defn- step-li [s static?]
  (let [kind (step-kind s)
        cls (cond (:err s) "err" (= :edit kind) "edit" (= :run kind) "run" :else "")
        head (list [:span {:class (str "chip " cls) :title (when (:err s) (str (:err s)))} (if (:err s) (exit-label (:err s)) (name kind))]
                   (clip (step-label s) 150)
                   (when (or (pos? (:add s 0)) (pos? (:del s 0))) (list " " [:span.add (str "+" (:add s))] " " [:span.del (str "−" (:del s))]))
                   (when-let [ms (get-in s [:trailers "Duration-Ms"])] [:small {:style "color:var(--muted)"} (str "  " ms " ms")]))]
    [:li
     (cond (seq (:files s))
           (if static?
             [:details [:summary head] (diff-pre (apply git "show" "--format=" "--patch" (:sha s) "--" "." not-trace) 12000)]
             [:details {:class "lazy" :data-src (str "/x/diff?c=" (:sha s)) :data-kind "diff"} [:summary head] [:div.lazy-body [:div.loading "loading…"]]])
           (:result s)
           (if static?
             [:details [:summary head] [:pre.body (clipped (git "cat-file" "-p" (:result s)) 4000)]]
             [:details {:class "lazy" :data-src (str "/x/show?r=" (:result s)) :data-kind "text"} [:summary head] [:div.lazy-body [:div.loading "loading…"]]])
           :else [:div.plain.plain head])]))

;; --- numbers side by side, and what differs ----------------------------------------------------

(defn- flat
  "The comparable numbers of a prompt's metrics or an arm's total."
  [m]
  (let [u (or (:usage m) (:usage (:turn m)))]
    {:calls (:calls m) :edits (:edits m) :failed (:failed m) :add (:add m) :del (:del m) :secs (:secs m)
     :input (:input_tokens u) :out (:output_tokens u) :think (:reasoning_tokens u) :cache (:cache_read_input_tokens u)
     :cost (:cost m)}))

(def ^:private stat-groups
  "[group [[key label better]]]: better is :lower (a smaller number is the better one) or nil (just different)."
  [["work" [[:calls "calls" :lower] [:edits "edits" nil] [:failed "failed" :lower]]]
   ["change" [[:add "added" nil] [:del "removed" nil]]]
   ["time" [[:secs "time" :lower]]]
   ["tokens" [[:out "output" :lower] [:think "thinking" nil] [:cache "cache read" :lower]]]
   ["cost" [[:cost "list price" :lower]]]])

(defn- show [k v]
  (cond (nil? v) "–"
        (= k :secs) (secs v)
        (= k :cost) (usd v)
        (= k :add) (if (zero? v) "0" (str "+" (thou v)))
        (= k :del) (if (zero? v) "0" (str "−" (thou v)))
        :else (thou v)))

(defn- sgn [x] (cond (pos? x) "+" (neg? x) "−" :else "±"))

(defn- delta-text [k v b]
  (when (and v b)
    (let [d (- v b)
          abs* (Math/abs (double d))
          body (cond (= k :secs) (secs abs*)
                     (= k :cost) (usd abs*)
                     :else (thou (long abs*)))
          pct (when (and (pos? b) (not (zero? d))) (format " (%s%d%%)" (sgn d) (Math/round (* 100.0 (/ abs* b)))))]
      (str (sgn d) (when-not (zero? d) body) pct))))

(defn- delta-class [k v b better]
  (let [d (when (and v b) (- v b))]
    (cond (or (nil? d) (zero? d) (nil? better)) "flat" (neg? d) "good" :else "bad")))

(defn- strip
  "The banner of one prompt (or run) of one arm: labelled groups, the same on every arm, with what differs from base (a flat map) under each number."
  [f base]
  [:div.strip
   (for [[g items] stat-groups]
     [:div.grp [:div.gl g]
      [:div.gi
       (for [[k label better] items]
         [:div.st [:div.sl label] [:div.sv (show k (get f k))]
          [:div {:class (str "sd " (when base (delta-class k (get f k) (get base k) better)))}
           (if base (delta-text k (get f k) (get base k)) "")]])]])])

;; --- the runs of a task ------------------------------------------------------------------------

(defn- stamp [secs] (when secs (.format (java.time.format.DateTimeFormatter/ofPattern "MMM d, HH:mm") (.atZone (java.time.Instant/ofEpochSecond secs) (java.time.ZoneId/systemDefault)))))

(defn task-runs
  "{:all [run ...] :shown [run ...]} of a task: every session of every arm is a run (arm-data + :check), oldest
  first within an arm. pick: a set of \"arm:sid8\"; the runs shown are those picked, and for an arm with none
  picked its newest."
  [tips task pick]
  (let [all (vec (for [[arm project] (get (tasks tips) task)
                       {:keys [sid8]} (sessions-of tips project)
                       :let [a (arm-data tips arm project sid8)] :when a]
                   (assoc a :check (check (:tip a)))))
        by-arm (group-by :arm all)
        shown (vec (mapcat (fn [[_ runs*]]
                             (or (seq (filter #(contains? pick (str (:arm %) ":" (:sid8 %))) runs*)) [(peek (vec runs*))]))
                           (sort-by key by-arm)))]
    {:all all :shown shown}))

(defn- run-id [a] (str (:arm a) ":" (:sid8 a)))

(defn- labelled
  "The shown runs with a :label (the arm, plus the time when an arm is shown twice) and the baseline marked:
  the cheapest run (the first when none has a price)."
  [shown]
  (let [n (frequencies (map :arm shown))
        base (or (when (some #(get-in % [:total :cost]) shown) (apply min-key #(or (get-in % [:total :cost]) Double/MAX_VALUE) shown)) (first shown))]
    (mapv #(assoc % :label (if (> (n (:arm %)) 1) (str (:arm %) " · " (stamp (:at %))) (:arm %)) :base? (identical? % base)) shown)))

(defn- summary-table [all shown {:keys [static? task]}]
  (let [units? (some #(pos? (or (get-in % [:total :usage :copilot_cost]) 0)) all)
        shown-ids (set (map run-id shown))
        multi? (> (count all) (count (distinct (map :arm all))))
        pick-href (fn [a] (let [now (if (shown-ids (run-id a)) (disj shown-ids (run-id a)) (conj shown-ids (run-id a)))]
                            (str "/runs/" (enc task) "?pick=" (enc (str/join "," (sort now))))))]
    [:table.arms
     [:thead [:tr [:th "run"] [:th "end state"] [:th "calls"] [:th "edits"] [:th "failed"] [:th "lines"] [:th "time"]
              [:th "input"] [:th "output"] [:th "thinking"] [:th "cache read"] [:th "cost"] (when units? [:th "Copilot units"])]]
     [:tbody
      (for [{:keys [arm total check] :as a} all
            :let [u (:usage total) on? (shown-ids (run-id a))]]
        [:tr {:class (when-not on? "older")}
         [:td [:b arm] (when multi? [:small " " (stamp (:at a))])
          (when (and multi? (not static?)) [:a.toggle {:href (pick-href a)} (if on? "shown · hide" "show")])]
         [:td (cond (nil? check) "–"
                    (zero? (:exit check)) [:span.pass (str "✓ " (or (:pass check) "") " pass")]
                    :else [:span.failx (str "✗ " (or (:fail check) "?") " fail")])]
         [:td (:calls total)] [:td (:edits total)] [:td (:failed total)]
         [:td [:span.add (str "+" (:add total))] " " [:span.del (str "−" (:del total))]]
         [:td (secs (:secs total))]
         [:td (thou (:input_tokens u))] [:td (thou (:output_tokens u))] [:td (thou (:reasoning_tokens u))]
         [:td (thou (:cache_read_input_tokens u))]
         [:td (usd (:cost total))]
         (when units? [:td (:copilot_cost u)])])]]))

(defn- delta-table
  "What each shown arm differs by from the baseline arm, one row per arm, a number per column of the summary."
  [arms]
  (let [base (first (filter :base? arms))
        bf (flat (:total base))
        cols [[:calls "calls" :lower] [:edits "edits" nil] [:failed "failed" :lower] [:add "added" nil] [:del "removed" nil]
              [:secs "time" :lower] [:input "input" :lower] [:out "output" :lower] [:think "thinking" nil] [:cache "cache read" :lower] [:cost "cost" :lower]]]
    (when (> (count arms) 1)
      (list
       [:h4 (str "Compared with " (:label base) " (the cheapest; green is less, red is more)")]
       [:table.arms.delta
        [:thead [:tr [:th "arm"] (for [[_ l _] cols] [:th l])]]
        [:tbody
         (for [a arms :when (not (:base? a)) :let [f (flat (:total a))]]
           [:tr [:td [:b (:label a)]]
            (for [[k _ better] cols]
              [:td {:class (delta-class k (get f k) (get bf k) better)} (if-let [t (delta-text k (get f k) (get bf k))]
                (if-let [i (str/index-of t " (")] (list (subs t 0 i) " " [:small (subs t (inc i))]) t)
                "–")])])]]))))

(defn- whole-change
  "[stat-line diff] of the whole run: the first prompt's start tree to the last commit."
  [a]
  (let [from (:fork (:d (first (:prompts a)))) to (:end (:d (peek (:prompts a))))]
    (when (and from to)
      {:stat (->> (str/split-lines (apply git "diff" "--numstat" from to "--" "." not-trace))
                  (keep #(let [[ad de p] (str/split % #"\t" 3)] (when p {:path p :add (or (parse-long ad) 0) :del (or (parse-long de) 0)}))))
       :diff (apply git "diff" from to "--" "." not-trace)})))

(defn- whole-section [arms]
  [:section#whole
   [:h2 "The whole change, start folder to final tree"]
   [:div.cols {:style (str "--n:" (count arms))}
    (for [a arms :let [w (whole-change a)]]
      [:div.col
       [:h3 (:label a) [:small (str (count (:stat w)) " files · +" (reduce + 0 (map :add (:stat w))) " −" (reduce + 0 (map :del (:stat w))))]]
       (if-not w
         [:div "nothing recorded"]
         (list
          [:table.files [:tbody (for [{:keys [path add del]} (:stat w)]
                                  [:tr [:td path] [:td.add (str "+" add)] [:td.del (str "−" del)]])]]
          [:details {:open true} [:summary "Full diff"] (diff-pre (:diff w) 200000)]))])]])

(defn- task-body [task all shown static?]
  (let [arms (labelled shown)
        n (apply max 0 (map #(count (:prompts %)) arms))
        base (first (filter :base? arms))]
    [:main.runs-wrap
     (if static?
       [:div.crumb "reus · runs of the same task"]
       [:div.crumb [:a {:href "/runs"} "Runs"] " / " task])
     [:h1 task]
     (if (empty? arms)
       [:p "No run of this task is recorded."]
       (list
        [:div.toolbar [:button {:type "button" :onclick "reusAll(true)"} "Expand all"] [:button {:type "button" :onclick "reusAll(false)"} "Collapse all"]]
        (summary-table all shown {:static? static? :task task})
        (delta-table arms)
        [:p.crumb "Cost is list price of the tokens (input counts what was not cached). The check is the command in the final tree's "
         [:code ".reus-check"] ", run on that tree."
         (when (> (count all) (count shown)) " Greyed rows are older runs of an arm; the columns below show the newest.")]
        (for [a arms :when (:check a)]
          [:details [:summary (str (:label a) ": " (:cmd (:check a)) " → exit " (:exit (:check a)))]
           [:pre.checkout (:out (:check a))]])
        (for [i (range n)
              :let [pd (some #(:d (nth (:prompts %) i nil)) arms)
                    bm (some-> (first (filter :base? arms)) :prompts (nth i nil) :m flat)]]
          [:section
           [:h2 (str "Prompt " (inc i))]
           [:div.ptext (:prompt pd)]
           [:div.cols {:style (str "--n:" (count arms))}
            (for [a arms :let [p (nth (:prompts a) i nil)]]
              [:div.col
               [:h3 (:label a) (when (:base? a) [:small "baseline"])
                (when (and p (not static?)) [:small [:a {:href (str "/p/" (enc (:id p)) "?m=" (enc (:project a)))} "open in trace"]])]
               (if-not p
                 [:div "not reached"]
                 (let [{:keys [d m]} p]
                   (list
                    (strip (flat m) (when-not (:base? a) bm))
                    [:ol.calls (for [s (:steps d)] (step-li s static?))]
                    (if static?
                      [:details [:summary "Net change of this prompt"]
                       (diff-pre (apply git "diff" (:fork d) (:end d) "--" "." not-trace) 40000)]
                      (lazy "Net change of this prompt" (str "/x/net?m=" (enc (:project a)) "&p=" (enc (:id p))) "diff"))
                    [:details.rr (when static? {:open true}) [:summary "Reply"] [:pre (:reply d)]])))])]])
        (whole-section arms)))]))

(defn- pick-set [q] (set (remove str/blank? (str/split (str (get q "pick")) #","))))

(defn- task-page [tips task q]
  (let [{:keys [all shown]} (task-runs tips task (pick-set q))]
    (page (str task " · runs") (task-body task all shown false))))

(defn static-html
  "The comparison of a task as ONE self-contained HTML file (inline css, every diff and output inline, one small inline
  script for expand/collapse all, no font or file from the net): the newest run of each arm in columns, all runs in
  the table. Reads the trace repo g/*repo*."
  [task]
  (let [tips (r/tips)
        {:keys [all shown]} (task-runs tips task #{})]
    (page (str task " · runs") (task-body task all shown true) :static? true)))

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
  "The response for /runs or /runs/<task>[?pick=arm:sid8,...], nil when the uri is not ours. q: the query params."
  [uri & [q]]
  (let [html (fn [s] {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body s})
        tips (r/tips)]
    (cond (= uri "/runs") (html (index-page tips))
          (re-matches #"/runs/[^/]+" uri)
          (let [t (java.net.URLDecoder/decode (subs uri 6) "UTF-8")]
            (if (get (tasks tips) t) (html (task-page tips t q)) {:status 404 :body "no such task"})))))
