(ns trace.worker
  "Makes the history from the spool (trace.hook writes it): every event, oldest first, through
  trace.record against the live store, under the store's write lock. One worker per trace repo (an OS
  lock on .git/trace-worker.lock: a second one exits at once, a dead one holds nothing); the hook
  starts it when none is running, it stays while events come and exits after idle-ms without any.

  An event that cannot be recorded goes to .git/trace-failed/ with the reason in trace.health
  (:worker) and the errors log, and the others go on: one bad event never stops the trace.
  `bb trace:retry` puts the failed ones back. A snapshot that was too slow for the hook is redone
  here without a time limit, so the worktree's index is warm for the next hook. Once an hour the
  worker lets git pack the repo (`gc --auto`)."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.disk :as disk]
            [trace.files :as files]
            [trace.git :as g]
            [trace.health :as health]
            [trace.record :as rec]
            [trace.store :as store]))

(def idle-ms "How long a worker waits for more events before it exits." 20000)
(def gc-every-ms (* 60 60 1000))

(defn- warm!
  "Hash a worktree whose snapshot was too slow for the hook, without a limit."
  [repo wt]
  (try (disk/live-snapshot repo wt {:ms nil}) (catch Exception e (health/log! "worker warm" "" {} e))))

(defn- record-entry! [repo f]
  (let [{:keys [event raw obs unreadable]} (try (edn/read-string (slurp f)) (catch Throwable e {:unreadable e}))
        input (try (json/parse-string raw) (catch Throwable _ {}))]
    (try
      (when unreadable (throw unreadable)) ; it goes aside like any other failure, the rest go on
      (rec/handle-in! {:s (store/live repo) :d (disk/live repo) :live? true} event input
                      {:raw raw :obs obs :entry (str (fs/file-name f))})
      (when (and (:slow? obs) (:common (:wt obs))) (warm! repo (:wt obs)))
      (fs/delete f)
      (health/ok! :worker)
      true
      (catch Throwable e
        (health/log! "worker" event input e)
        (health/fail! :worker event input e)
        (fs/create-dirs (files/failed-dir repo))
        (fs/move f (fs/path (files/failed-dir repo) (fs/file-name f)) {:replace-existing true})
        false))))

(defn- pause-file [repo] (str (fs/path repo ".git" "trace-worker.pause")))

(defn- drain*
  "Record every spooled event of repo, oldest first, until the spool is empty; how many were recorded."
  [repo]
  (binding [g/*repo* (str repo)]
    (loop [n 0]
      (if-let [es (when-not (fs/exists? (pause-file repo)) (seq (files/entries (files/spool-dir repo))))]
        (recur (+ n (count (filter true? (map #(record-entry! repo %) es)))))
        n))))

(defn- wait-empty [repo ms]
  (loop [n 0]
    (when (and (seq (files/entries (files/spool-dir repo))) (< (* n 100) ms))
      (Thread/sleep 100)
      (recur (inc n)))))

(defn drain!
  "Record what is spooled now, in this process: how many. When a worker is draining already, wait (at
  most ms, 60 s) for it to empty the spool instead: 0."
  [repo & [ms]]
  (or (files/try-file-lock (files/worker-lock repo) #(drain* repo))
      (do (wait-empty repo (or ms 60000)) 0)))

(defn- sweep!
  "Temp files a hook killed mid-write left in the spool (its event is lost; the file must not stay)."
  [repo]
  (let [d (files/spool-dir repo) old (- (System/currentTimeMillis) 60000)]
    (when (fs/directory? d)
      (doseq [f (fs/list-dir d) :when (and (files/tmp? f) (< (.toMillis (fs/last-modified-time f)) old))]
        (fs/delete-if-exists f)))))

(defn- gc! [repo]
  (let [stamp (files/kv-read repo :prev "gc-at")
        now (System/currentTimeMillis)]
    (when (or (nil? stamp) (> (- now (or (parse-long stamp) 0)) gc-every-ms))
      (files/kv-write! repo :prev "gc-at" (str now))
      (sweep! repo)
      (let [r (p/shell {:dir (str repo) :out :string :err :string :continue true} "git" "gc" "--auto" "--quiet")]
        (when-not (and (zero? (:exit r)) (str/blank? (:err r)))
          (health/log! "worker gc" "" {} (ex-info (str "git gc --auto: exit " (:exit r) " " (str/trim (str (:err r)))) {})))))))

(defn run!
  "Drain repo while events come; return after idle-ms without any. Nil at once when another worker drains it."
  [repo]
  (loop []
    (files/try-file-lock
     (files/worker-lock repo)
     (fn []
       (loop [idle-since (System/currentTimeMillis)]
         (let [n (drain* repo)
               now (System/currentTimeMillis)]
           (cond (fs/exists? (pause-file repo)) nil ; own-objects! wants the repo to itself
                 (pos? n) (recur now)
                 (not (fs/exists? (fs/path repo ".git" "HEAD"))) nil ; the repo is gone
                 (> (- now idle-since) idle-ms) nil
                 :else (do (gc! repo) (Thread/sleep 50) (recur idle-since)))))))
    ;; a hook that spooled while this worker let go saw it still running: look once more
    (when (and (seq (files/entries (files/spool-dir repo)))
               (not (fs/exists? (pause-file repo)))
               (files/try-file-lock (files/worker-lock repo) (constantly true)))
      (recur))))

(defn- git-ok [repo & args]
  (let [r (apply p/shell {:dir (str repo) :out :string :err :string :continue true} "git" args)]
    (when-not (zero? (:exit r)) (throw (ex-info (str "git " (first args) ": " (str/trim (str (:err r)))) {})))
    (:out r)))

(defn- complete? [repo]
  (let [r (p/shell {:dir (str repo) :out :string :err :string :continue true} "git" "fsck" "--connectivity-only" "--no-dangling")]
    (and (zero? (:exit r)) (not (re-find #"(?m)^(missing|broken|error)" (str (:out r) (:err r)))))))

(defn own-objects!
  "Make a trace repo of older code hold every object it refers to and borrow none: pack everything
  reachable (borrowed objects copied in), check it is complete, drop objects/info/alternates (kept as
  alternates.bak), check again without them (else they are put back), drop the private indexes seeded
  from the project repos and the state no code reads any more. The worker is paused meanwhile (events
  wait in the spool) and drains after. bb trace:own-objects; says what it did."
  [repo]
  (let [git-dir (fs/path repo ".git")
        alt (fs/path git-dir "objects" "info" "alternates")
        say #(do (println %) (flush))]
    (fs/create-file (pause-file repo))
    (try
      (files/with-file-lock (files/worker-lock repo) nil
        (fn []
          (say (str "before: " (str/replace (git-ok repo "count-objects" "-vH") "\n" " ")))
          (git-ok repo "repack" "-a" "-d" "-q")
          (when-not (complete? repo) (throw (ex-info "incomplete after the repack: alternates kept" {})))
          (when (fs/exists? alt)
            (fs/move alt (str alt ".bak") {:replace-existing true})
            (if (complete? repo)
              (say "alternates dropped (kept as objects/info/alternates.bak)")
              (do (fs/move (str alt ".bak") alt {:replace-existing true})
                  (throw (ex-info "objects missing without the alternates: put back" {})))))
          (doseq [f (fs/list-dir (fs/path git-dir "trace-index")) :when (not (str/ends-with? (str f) ".oslock"))]
            (fs/delete-if-exists f))
          (doseq [d ["trace.lock" "trace-note-index" "trace-index-tmp" "trace-roots" "trace-health.edn" "trace-tmp"]]
            (fs/delete-tree (fs/path git-dir d)))
          (say (str "after: " (str/replace (git-ok repo "count-objects" "-vH") "\n" " ")))))
      (finally (fs/delete-if-exists (pause-file repo))))
    (say (str (drain! repo) " waiting events recorded"))))

(defn retry!
  "Spool the failed events again (bb trace:retry); how many."
  [repo]
  (let [fs* (files/entries (files/failed-dir repo))]
    (doseq [f fs*] (fs/move f (fs/path (files/spool-dir repo) (fs/file-name f)) {:replace-existing true}))
    (count fs*)))

(defn -main [& _]
  (try (run! g/*repo*)
       (catch Throwable e (health/log! "worker" "" {} e) (health/fail! :worker "" {} e)))
  (System/exit 0))
