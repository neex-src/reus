(ns trace.health
  "Is the trace working? Read from files of the trace repo only (no git, nothing heavy: a status bar
  runs it every few seconds).

  .git/trace-health/hook.edn, worker.edn   {:failing-since :reason :event :session}: the first failing
                                          time of the failure going on, the latest reason; deleted by
                                          the next event that succeeds in that part
  .git/trace-spool/                       events not recorded yet (behind: the oldest is over a minute old)
  .git/trace-failed/                      events the worker could not record (bb trace:retry spools them again)

  (line)   what bb trace:health prints: \"trace ok\", else one line per problem
  (short)  for a status bar: empty while ok"
  (:refer-clojure :exclude [short])
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.files :as files]
            [trace.git :as g]))

(def behind-ms "A spooled event this old means the worker is not keeping up (or not running)." 60000)

(defn- file [part] (str (fs/path g/*repo* ".git" "trace-health" (str (name part) ".edn"))))

(defn fail!
  "Part (:hook or :worker) failed on an event: the first failing-since is kept while it goes on."
  [part event input e]
  (let [f (file part)
        since (or (try (:failing-since (edn/read-string (slurp f))) (catch Throwable _ nil))
                  (str (java.time.Instant/now)))]
    (files/spit-atomic f (pr-str {:failing-since since :reason (or (ex-message e) (str e)) :event event
                                  :session (get input "session_id")}))))

(defn ok!
  "Part succeeded: healthy again."
  [part]
  (fs/delete-if-exists (file part)))

(defn log!
  "One line in .git/trace-errors.log."
  [what event input e]
  (try (spit (str (fs/path g/*repo* ".git" "trace-errors.log"))
             (str (java.time.Instant/now) " " what " " event " " (get input "session_id") " " (or (ex-message e) (str e)) "\n")
             :append true)
       (catch Throwable _)))

(defn state
  "{:hook {...} :worker {...} :waiting n :oldest-ms :failed n}."
  []
  (let [read #(try (edn/read-string (slurp (file %))) (catch Throwable _ nil))
        waiting (files/entries (files/spool-dir g/*repo*))
        failed (files/entries (files/failed-dir g/*repo*))]
    {:hook (read :hook) :worker (read :worker)
     :waiting (count waiting) :oldest-ms (some-> (first waiting) files/entry-ms)
     :failed (count failed)}))

(defn- local [ms-or-iso]
  (try (.format (java.time.LocalDateTime/ofInstant (if (string? ms-or-iso) (java.time.Instant/parse ms-or-iso) (java.time.Instant/ofEpochMilli ms-or-iso))
                                                   (java.time.ZoneId/systemDefault))
                (java.time.format.DateTimeFormatter/ofPattern "MM-dd HH:mm"))
       (catch Throwable _ (str ms-or-iso))))

(defn problems
  "[[label since reason]] of what is wrong now."
  []
  (let [{:keys [hook worker waiting oldest-ms failed]} (state)]
    (cond-> []
      hook (conj ["hook failing" (:failing-since hook) (:reason hook)])
      worker (conj ["worker failing" (:failing-since worker) (:reason worker)])
      (and oldest-ms (> (- (System/currentTimeMillis) oldest-ms) behind-ms))
      (conj ["behind" oldest-ms (str waiting " events not recorded yet")])
      (pos? failed) (conj ["events failed" nil (str failed " in .git/trace-failed (bb trace:retry)")]))))

(defn line []
  (if-let [ps (seq (problems))]
    (str/join "\n" (for [[label since reason] ps]
                     (str "trace " label (when since (str " since " (if (number? since) (local since) since))) ": " reason)))
    "trace ok"))

(defn short []
  (if-let [[[label since reason] & more] (seq (problems))]
    (let [reason (str/replace (str reason) #"\s+" " ")]
      (str "TRACE " (str/upper-case label) (when since (str " since " (local since))) ": "
           (cond-> reason (> (count reason) 60) (-> (subs 0 60) (str "…")))
           (when more (str " (+" (count more) ")"))))
    ""))
