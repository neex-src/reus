(ns trace.health
  "Is the trace working? Read from files of the trace repo only (no git, nothing heavy: a status bar
  runs it every few seconds).

  .git/trace-health/hook.edn, worker.edn   {:failing-since :reason :event :session}: the first failing
                                          time of the failure going on, the latest reason; deleted by
                                          the next event that succeeds in that part
  .git/trace-spool/                       events not recorded yet (behind: the oldest is over a minute old)
  .git/trace-failed/                      events the worker could not record (bb trace:retry spools them again)
  <reus>/var/hook-seen/<session id>       a session the hook ran for (once per session): a hook that never
                                          runs fails nowhere (Claude Code drops a hook whose command is not
                                          found, without a word), so a transcript with no mark is the sign

  (line)   what bb trace:health prints: \"trace ok\", else one line per problem
  (short)  for a status bar: empty while ok"
  (:refer-clojure :exclude [short])
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.files :as files]
            [trace.git :as g]))

(def ^:dynamic *seen-dir*
  "Where the hook marks the sessions it ran for: $TRACE_SEEN, else var/hook-seen of the main checkout
  (one per machine, whatever trace repo or worktree of reus the hook ran from)."
  (or (System/getenv "TRACE_SEEN") (str (fs/path (g/main-checkout) "var" "hook-seen"))))

(defn seen!
  "The hook ran for this session. The first mark ever also writes .since: transcripts older than the
  marks are not looked at."
  [session]
  (when-not (str/blank? (str session))
    (let [f (fs/path *seen-dir* (files/clean-key session))]
      (when-not (fs/exists? f)
        (fs/create-dirs *seen-dir*)
        (let [since (fs/path *seen-dir* ".since")] (when-not (fs/exists? since) (spit (str since) "")))
        (spit (str f) "")))))

(def unseen-ms "How far back transcripts are looked at for sessions the hook never ran for." (* 24 3600 1000))
(def idle-ms "A transcript written to in the last minute may be of a session whose first event is under way." 60000)

(def ^:dynamic *claude-dir* (or (System/getenv "CLAUDE_CONFIG_DIR") (str (fs/path (fs/home) ".claude"))))

(defn seen?
  "Did the hook run for this session? For a harness that starts sessions with --no-session-persistence
  (no transcript: unrecorded cannot see them) and wants to know they were recorded."
  [session]
  (fs/exists? (fs/path *seen-dir* (files/clean-key session))))

(defn- installed? []
  (try (str/includes? (slurp (str (fs/path *claude-dir* "settings.json"))) "trace.hook") (catch Throwable _ false)))

(defn- last-entry-ms
  "When a transcript's last entry was written (its timestamp), nil when its tail has none. An old transcript can get
  a new mtime with nothing added to it."
  [^java.io.File f]
  (with-open [raf (java.io.RandomAccessFile. f "r")]
    (let [n (.length raf)
          off (max 0 (- n 65536))
          buf (byte-array (- n off))]
      (.seek raf off)
      (.readFully raf buf)
      (some-> (last (re-seq #"\"timestamp\":\"([^\"]+)\"" (String. buf "UTF-8")))
              second
              (#(try (.toEpochMilli (java.time.Instant/parse %)) (catch Throwable _ nil)))))))

(defn unrecorded
  "[{:session :file :ms}] of the transcripts (~/.claude/projects/*/*.jsonl) written in the last unseen-ms,
  idle for idle-ms, of sessions the hook never ran for, newest first; none while the hook is not installed
  (bb trace:off) or before it marked anything. Written means an entry was added, not that the file was touched. A session started with --no-session-persistence leaves
  no transcript and is not seen here."
  []
  (let [since (fs/file (fs/path *seen-dir* ".since"))]
    (when (and (.exists since) (installed?))
      (let [now (System/currentTimeMillis)
            from (max (.lastModified since) (- now unseen-ms))]
        (->> (.listFiles (fs/file *claude-dir* "projects"))
             (mapcat #(.listFiles ^java.io.File %))
             (keep (fn [^java.io.File f]
                     (let [n (.getName f) ms (.lastModified f)]
                       (when (and (str/ends-with? n ".jsonl") (> ms from) (< ms (- now idle-ms)))
                         (let [session (subs n 0 (- (count n) 6))]
                           (when (and (not (.exists (fs/file *seen-dir* (files/clean-key session))))
                                      (let [e (last-entry-ms f)] (or (nil? e) (> e from))))
                             {:session session :file (str f) :ms ms}))))))
             (sort-by :ms >))))))

(defn ack!
  "Mark the sessions unrecorded lists as seen (run without the hook on purpose: --safe-mode, other
  settings); what it marked."
  []
  (doall (for [{:keys [session] :as u} (unrecorded)] (do (seen! session) u))))

(def behind-ms "A spooled event this old means the worker is not keeping up (or not running)." 60000)

(defn- file [part] (str (fs/path g/*repo* ".git" "trace-health" (str (name part) ".edn"))))

(def clears-ms "A failure of one worktree not seen again this long is cleared by any success." (* 10 60 1000))

(defn- read-part [part] (try (edn/read-string (slurp (file part))) (catch Throwable _ nil)))

(defn fail!
  "Part (:hook or :worker) failed on an event (of the worktree root, when it is about one): the first
  failing-since is kept while it goes on."
  [part event input e & [root]]
  (let [since (or (:failing-since (read-part part)) (str (java.time.Instant/now)))]
    (files/spit-atomic (file part) (pr-str {:failing-since since :at (System/currentTimeMillis) :root root
                                            :reason (or (ex-message e) (str e)) :event event
                                            :session (get input "session_id")}))))

(defn ok!
  "Part succeeded (on worktree root): healthy again, unless what failed is another worktree that
  failed in the last clears-ms (one sick worktree stays visible while the others are fine)."
  [part & [root]]
  (when-let [h (read-part part)]
    (when (or (nil? (:root h)) (= root (:root h))
              (> (- (System/currentTimeMillis) (or (:at h) 0)) clears-ms))
      (fs/delete-if-exists (file part)))))

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
  (let [{:keys [hook worker waiting oldest-ms failed]} (state)
        unrecorded (try (unrecorded) (catch Throwable _ nil))]
    (cond-> []
      hook (conj ["hook failing" (:failing-since hook) (:reason hook)])
      worker (conj ["worker failing" (:failing-since worker) (:reason worker)])
      (and oldest-ms (> (- (System/currentTimeMillis) oldest-ms) behind-ms))
      (conj ["behind" oldest-ms (str waiting " events not recorded yet")])
      (pos? failed) (conj ["events failed" nil (str failed " in .git/trace-failed (bb trace:retry)")])
      (seq unrecorded)
      (conj ["sessions not recorded" (:ms (last unrecorded))
             (str (count unrecorded) " in 24 h, the hook never ran: " (str/join ", " (map #(subs (:session %) 0 (min 8 (count (:session %)))) unrecorded))
                  " (" (:file (first unrecorded)) "; bb trace:health --ack when on purpose)")]))))

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
