(ns trace.call
  "One model call, explained to someone who does not read traces: a session of ONE prompt, the shape of
  `claude -p --model haiku` as Deliberator's Generate starts it (the sandbox hook records it as the session branch
  <project>/s/<first 8 of the session id>).

    /calls                         the sessions of one prompt, newest first: when, model, task, tokens, cost, seconds
    /call/<session8>[@NN]          one call: what went in (the task, the prose, the size of the request and of the
                                   universal JSON), what the model looked at (each Read/Grep/Glob, in order, what
                                   came back), what came back (the steps), tokens read / cached / written / out,
                                   cost, seconds, the model's name
    ?m=<project>                   the session branch of that project, when several share the id
    ?run=<Generate run dir>        the CLI's own numbers, from that run's usage-N.edn whose :session-id is this
                                   session; they become the page's numbers and the page says whether reus counted
                                   the same. Only generate.edn, usage-N.edn and context/universal.json are read.

  Everything else is read from the trace repo (trace.read), nothing here is on the recording path. The page is
  self-contained (no scripts, no fonts from elsewhere, no scroll areas of its own): it is filmed in a box with no
  business reaching the internet. Cost: trace.runs/cost, list price; with the CLI's numbers, the CLI's cost."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [trace.git :as g :refer [git]]
            [trace.read :as r]
            [trace.runs :as runs])
  (:import [java.net URLEncoder]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

;; --- the data ---------------------------------------------------------------------------------

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))
(defn- project-of [branch] (second (re-matches #"(.+)/s/[^/]+" branch)))
(defn- sid8-of [branch] (second (re-matches #".+/s/([^/]+)" branch)))

(defn branches
  "The session branches (not agent runs) of session8, newest tip first: [[branch sha]]."
  [tips session8]
  (->> tips (filter (fn [[b _]] (= session8 (sid8-of b)))) vec))

(defn cli-usage
  "The CLI's own record of this session in a Generate run dir: that run's usage-N.edn with the session's id,
  with :file; nil when the dir has none (or is not a run dir)."
  [run-dir session]
  (when (and run-dir session (fs/exists? (fs/path run-dir "generate.edn")))
    (some (fn [f] (let [u (try (edn/read-string (slurp (str f))) (catch Exception _ nil))]
                    (when (= session (:session-id u)) (assoc u :file (str f)))))
          (sort (fs/glob run-dir "usage-*.edn")))))

(defn- rel
  "p relative to the session's cwd: trace.read already made commands relative to the session's root, so the
  cwd is tried as it is and as it is below the root."
  [{:keys [cwd root]} p]
  (let [p (str p)
        crel (when (and cwd root (str/starts-with? cwd (str root "/"))) (subs cwd (inc (count root))))]
    (or (some #(when (and % (str/starts-with? p (str % "/"))) (subs p (inc (count %)))) [cwd crel]) p)))

(defn- what-it-did
  "A look of the model in words: [verb detail]. where: {:cwd :root} of the session."
  [where {:keys [tool input]}]
  (case tool
    "Read" ["Read" (str (rel where (get input "file_path"))
                        (when-let [o (get input "offset")] (str ", lines " o " to " (+ o (or (get input "limit") 0)))))]
    "Grep" ["Searched" (str (rel where (or (get input "path") ".")) " for “" (get input "pattern") "”")]
    "Glob" ["Listed files" (str (get input "pattern"))]
    [(str tool) (str/join ", " (map (fn [[k v]] (str k " " v)) input))]))

(defn answer-steps
  "The head lines of the steps in an answer (`### kind verb target name`)."
  [reply]
  (mapv second (re-seq #"(?m)^### (.+)$" (str reply))))

(defn prose-of
  "The description a prompt carries, after Generate's marker; nil for a prompt that has none."
  [prompt]
  (when-let [i (str/index-of (str prompt) "The description to turn into steps:")]
    (let [t (subs prompt (+ i (count "The description to turn into steps:")))
          t (first (str/split t #"\n\nNow " 2))]
      (not-empty (str/trim t)))))

(defn call-data
  "Everything the page says about one call, from the prompt-data of its session [{:keys [tips branch nn run]}]:
  the numbers are reus's own, :cli the CLI's when a run dir named the session's usage."
  [{:keys [tips branch nn run]}]
  (let [main (project-of branch)
        d (r/prompt-data tips main (str (sid8-of branch) "@" nn))
        steps (:steps d)
        model (some #(get-in % [:trailers "Model"]) steps)
        usage (:usage (:turn-end d))
        cli (cli-usage run (:session d))
        model (or model (:model cli))
        universal (or (when run (let [f (fs/path run "context" "universal.json")] (when (fs/exists? f) (fs/size f))))
                      (some->> steps (filter #(some-> (:read-file %) (str/ends-with? "universal.json"))) first :blob
                               (#(parse-long (str (git "cat-file" "-s" %))))))]
    (when d
      {:branch branch :project main :session (:session d) :cwd (:cwd d) :at (:t0 d) :secs (when (:t1 d) (- (:t1 d) (:t0 d)))
       :model model :prompt (:prompt d) :reply (:reply d) :done? (some? (:reply d))
       :task (some-> (:prompt d) str/split-lines first)
       :prose (prose-of (:prompt d))
       :universal-bytes universal
       :looked (mapv (fn [s] (let [[verb detail] (what-it-did (select-keys d [:cwd :root]) (:call s))]
                               {:verb verb :detail detail :lines (:lines s) :silent (:silent s) :err (:err s)
                                :ms (some-> (get-in s [:trailers "Duration-Ms"]) parse-long)}))
                     steps)
       :answer-steps (answer-steps (:reply d))
       :rounds (:assistant-messages (:turn-end d))
       :usage usage
       :cost (runs/cost {:model model :usage usage})
       :cli (when cli
              (let [u (:usage cli)]
                {:in-fresh (:input-fresh u) :cache-read (:cache-read u) :cache-write (:cache-write u) :out (:output u)
                 :cost (:cost-usd u) :secs (some-> (:took-ms cli) (/ 1000.0)) :model (:model cli) :file (:file cli)
                 :agree? (and usage (= [(:input-fresh u) (:cache-read u) (:cache-write u) (:output u)]
                                       [(:input_tokens usage) (:cache_read_input_tokens usage)
                                        (:cache_creation_input_tokens usage) (:output_tokens usage)]))}))})))

;; --- the page ---------------------------------------------------------------------------------

(defn- thou [n] (if n (str/replace (str n) #"\B(?=(\d{3})+(?!\d))" ",") "–"))
(defn- usd [x] (cond (nil? x) "–" (< x 0.01) (format "$%.4f" (double x)) :else (format "$%.3f" (double x))))
(defn- stamp [secs]
  (when secs (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm") (.atZone (Instant/ofEpochSecond secs) (ZoneId/systemDefault)))))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "…") s)))
(defn- model-name [m] (some-> m (str/replace #"^claude-" "")))

(def ^:private css
  "body{margin:0;font:16px/1.5 system-ui,-apple-system,'Segoe UI',sans-serif;color:#1b1f24;background:#f6f7f9}
header.top{background:#1b1f24;color:#fff;padding:10px 24px;display:flex;gap:20px;align-items:baseline}
header.top a{color:#cfd6df;text-decoration:none}header.top a.brand{color:#fff;font-weight:600}
main{max-width:60rem;margin:0 auto;padding:24px 24px 64px}
h1{font-size:1.5rem;margin:.2em 0 .1em}h2{font-size:1.1rem;margin:2em 0 .4em;border-bottom:1px solid #d5dae1;padding-bottom:.2em}
.lede{font-size:1.15rem;margin:.4em 0 1em}
.nums{display:flex;flex-wrap:wrap;gap:12px;margin:12px 0}
.num{background:#fff;border:1px solid #d5dae1;border-radius:8px;padding:10px 16px;min-width:8.5rem}
.num b{display:block;font-size:1.6rem;line-height:1.2}.num span{color:#59636e;font-size:.85rem}
table{border-collapse:collapse;width:100%;background:#fff;border:1px solid #d5dae1}
th,td{text-align:left;padding:6px 10px;border-bottom:1px solid #e6e9ee;vertical-align:top}th{background:#eef1f5;font-weight:600}
td.n,th.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
pre{white-space:pre-wrap;word-break:break-word;background:#fff;border:1px solid #d5dae1;border-radius:6px;padding:10px 12px;margin:.4em 0;font:13px/1.45 ui-monospace,Menlo,Consolas,monospace}
.quote{background:#fff;border-left:4px solid #3b6fd4;padding:8px 14px;margin:.4em 0;white-space:pre-wrap}
.muted{color:#59636e}.ok{color:#17803d}.bad{color:#b42318}
ol.steps{margin:.3em 0;padding-left:1.4em}ol.steps li{margin:.15em 0}
code{font:13px ui-monospace,Menlo,Consolas,monospace}
details summary{cursor:pointer;color:#3b6fd4}
.src{color:#59636e;font-size:.85rem;margin-top:2.5em}")

(defn- shell [title body]
  (str "<!doctype html>"
       (h/html
        [:html {:lang "en"}
         [:head [:meta {:charset "utf-8"}] [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
          [:title title] [:style (h/raw css)]]
         [:body
          [:header.top [:a.brand {:href "/"} "reus trace"] [:a {:href "/calls"} "Model calls"] [:a {:href "/runs"} "Runs"]]
          [:main body]]])))

(defn- num [big small] [:div.num [:b big] [:span small]])

(defn- call-page [{:keys [model at secs prompt task prose universal-bytes looked reply answer-steps rounds usage cost cli done? branch session] :as c}]
  (let [fresh (if cli (:in-fresh cli) (:input_tokens usage))
        cread (if cli (:cache-read cli) (:cache_read_input_tokens usage))
        cwrite (if cli (:cache-write cli) (:cache_creation_input_tokens usage))
        out (if cli (:out cli) (:output_tokens usage))
        total-in (when (and fresh cread cwrite) (+ fresh cread cwrite))
        cost* (if cli (:cost cli) cost)
        secs* (if cli (:secs cli) secs)
        mname (model-name model)]
    (shell
     (str "Model call " (subs (str session) 0 (min 8 (count (str session)))))
     (list
      [:h1 "One request to a language model"]
      [:p.muted (stamp at) " · " (str "session " (subs (str session) 0 (min 8 (count (str session))))) " · model " [:b mname]]
      (if-not (and usage done?)
        [:p.lede "This call is not finished yet, or reus did not see its end."]
        [:p.lede "The software sent " [:b mname] " a request of " [:b (thou total-in)] " tokens "
         "(" (thou cread) " of them re-used from its cache, so they cost almost nothing), it looked things up "
         [:b (count looked)] (if (= 1 (count looked)) " time" " times") " and wrote back " [:b (thou out)] " tokens. "
         "That cost " [:b (usd cost*)] (when secs* (list " and took " [:b (format "%.0f" (double secs*))] (if (< (Math/round (double secs*)) 2) " second" " seconds"))) "."])
      [:div.nums
       (num (thou total-in) "tokens in (everything the model read)")
       (num (thou out) "tokens out (what it wrote, thinking included)")
       (num (thou cread) "of the tokens in: re-used from cache")
       (num (usd cost*) (str "cost at list price" (when cli " (the CLI's own figure)")))
       (num (if secs* (format "%.0f s" (double secs*)) "–") "from request to answer")
       (num mname "the model")]
      [:h2 "1. What went in"]
      [:p "The task, as the software wrote it:"]
      [:div.quote (or task "–")]
      (when prose
        (list [:p "The description a person wrote in prose, which the model had to turn into steps:"]
              [:div.quote prose]))
      [:p "The whole request is " [:b (thou (count (str prompt)))] " characters of instructions"
       (when universal-bytes (list ", and the model could read " [:b (thou universal-bytes)] " bytes of "
                                   [:b "universal JSON"] " (everything the workspace knows about itself)"))
       ". "
       [:details {:style "display:inline"} [:summary "Show the whole request"] [:pre prompt]]]
      [:h2 "2. What the model looked at"]
      (if (seq looked)
        [:table
         [:thead [:tr [:th "#"] [:th "it did"] [:th "what"] [:th.n "came back"] [:th.n "took"]]]
         [:tbody
          (for [[i s] (map-indexed vector looked)]
            [:tr [:td (inc i)] [:td (:verb s)] [:td [:code (clip (:detail s) 150)]]
             [:td.n (cond (:err s) [:span.bad "an error"] (:silent s) "nothing" (:lines s) (str (thou (:lines s)) (if (= 1 (:lines s)) " line" " lines")) :else "–")]
             [:td.n (if (:ms s) (str (:ms s) " ms") "–")]])]]
        [:p.muted "It answered without looking at anything."])
      (when rounds [:p.muted "The software and the model talked in " rounds " rounds: the looks above, then the answer."])
      [:h2 "3. What came back"]
      (if reply
        (list
         [:p [:b (count answer-steps)] (if (= 1 (count answer-steps)) " step" " steps")
          " of a test scenario, as plain text. The software checks them and puts them into the file; the model changed nothing itself."]
         (when (seq answer-steps)
           [:ol.steps (for [s answer-steps] [:li [:code (clip s 140)]])])
         [:details [:summary "Show the answer"] [:pre reply]])
        [:p.muted "No answer recorded."])
      [:h2 "4. What it cost"]
      [:table
       [:thead [:tr [:th "tokens"] [:th.n "count"] [:th "what it is"]]]
       [:tbody
        [:tr [:td "in, fresh"] [:td.n (thou fresh)] [:td "the part of the request that was new to the model"]]
        [:tr [:td "in, from cache"] [:td.n (thou cread)] [:td "instructions and tools it had already been given, re-read at a tenth of the price"]]
        [:tr [:td "in, written to cache"] [:td.n (thou cwrite)] [:td "stored so the next round does not pay for it again"]]
        [:tr [:td "out"] [:td.n (thou out)] [:td "everything the model wrote, its own thinking included"]]
        [:tr [:th "cost"] [:th.n (usd cost*)] [:td.muted (if cli "as the Claude CLI reported it" "tokens at the model's list price; the work ran on a subscription, nobody was invoiced")]]]]
      (when cli
        [:p (if (:agree? cli)
              [:span.ok "reus counted the same tokens, from its own record of the session."]
              [:span.bad "reus's own record of this call differs: it says "
               (thou (:output_tokens usage)) " out and " (thou (:input_tokens usage)) " fresh in, the CLI " (thou (:out cli)) " and " (thou (:in-fresh cli))
               ". That record was made before reus read the end of a call to the end; the CLI's figures are the ones above."])])
      [:p.src "Source: the reus trace repo " [:code g/*repo*] ", branch " [:code branch] ". "
       (if cli (list "Numbers: " [:code (:file cli)] ". ") "Numbers: the transcript of the session as reus recorded it. ")
       "Model calls: " [:a {:href "/calls"} "all of them"] "."]))))

(defn- calls-page [rows]
  (shell "Model calls"
         (list
          [:h1 "Model calls"]
          [:p.muted "Sessions of a single request, newest first. Pick one to see what went in, what the model looked at and what came back."]
          [:table
           [:thead [:tr [:th "when"] [:th "model"] [:th "task"] [:th.n "tokens in"] [:th.n "tokens out"] [:th.n "cost"] [:th.n "seconds"]]]
           [:tbody
            (for [{:keys [at model task usage cost secs branch session]} rows
                  :let [href (str "/call/" (sid8-of branch) "?m=" (enc (project-of branch)))]]
              [:tr [:td (stamp at)] [:td (model-name model)] [:td [:a {:href href} (clip task 100)]]
               [:td.n (thou (when usage (+ (or (:input_tokens usage) 0) (or (:cache_read_input_tokens usage) 0) (or (:cache_creation_input_tokens usage) 0))))]
               [:td.n (thou (:output_tokens usage))] [:td.n (usd cost)] [:td.n (if secs (str secs) "–")]])]]
          (when (empty? rows) [:p "No single-request sessions in this trace repo yet."]))))

(defn- one-prompt-branches
  "The newest `n` session branches that hold exactly one prompt: [[branch sha]]."
  [tips n]
  (->> (str/split-lines (git "for-each-ref" "--sort=-committerdate" "--format=%(refname:short)" "refs/heads"))
       (filter #(re-matches #".+/s/[^/]+" %))
       (take 200)
       (filter (fn [b] (= 1 (count (filter #(re-find #"^prompt [\w-]+:" (:subject %)) (r/own-commits (get tips b)))))))
       (take n)
       (map (fn [b] [b (get tips b)]))))

;; --- routes -----------------------------------------------------------------------------------

(defn- html [s] {:status 200 :headers {"content-type" "text/html; charset=utf-8" "cache-control" "no-store"} :body s})

(defn route
  "The response for /calls or /call/<session8>[@NN] (q: the query params as a map), nil when the uri is not ours."
  [uri q]
  (let [tips (r/tips)]
    (cond
      (= uri "/calls")
      (html (calls-page (keep #(call-data {:tips tips :branch (first %) :nn "00"}) (one-prompt-branches tips 40))))

      (str/starts-with? uri "/call/")
      (let [[sid nn] (str/split (subs uri 6) #"@" 2)
            bs (branches tips sid)
            b (or (some #(when (= (get q "m") (project-of (first %))) %) bs) (first bs))]
        (if-let [c (and b (call-data {:tips tips :branch (first b) :nn (or nn "00") :run (not-empty (get q "run"))}))]
          (html (call-page c))
          {:status 404 :headers {"content-type" "text/plain; charset=utf-8"} :body (str "no such call: " sid)})))))
