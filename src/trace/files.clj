(ns trace.files
  "Small file primitives the hook and the worker share: an OS lock and a write that lands whole or not
  at all. No git, nothing heavy: the hook loads this."
  (:require [babashka.fs :as fs]
            [clojure.edn]
            [clojure.string]))

(defn- open-channel ^java.nio.channels.FileChannel [file]
  (java.nio.channels.FileChannel/open (java.nio.file.Paths/get (str file) (into-array String []))
                                      (into-array java.nio.file.StandardOpenOption
                                                  [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE])))

(defn- try-lock
  "The lock, or nil when another process holds it (or this one does: OverlappingFileLockException)."
  [^java.nio.channels.FileChannel ch]
  (try (.tryLock ch) (catch IllegalStateException _ nil)))

(defn with-file-lock
  "Call f holding an OS lock on file (made when missing). The kernel releases it when this process
  dies, so a lock is never stale and never taken from a live holder. ms: wait at most that long
  (nil: as long as it takes); not got in time: (on-timeout), f is not called."
  [file ms f & [on-timeout]]
  (fs/create-dirs (fs/parent file))
  (with-open [ch (open-channel file)]
    (let [deadline (when ms (+ (System/currentTimeMillis) ms))
          lock (loop []
                 (or (try-lock ch)
                     (when (or (nil? deadline) (< (System/currentTimeMillis) deadline))
                       (Thread/sleep 10)
                       (recur))))]
      (if lock
        (f) ; closing the channel (with-open) releases the lock (FileLock.release is not open to bb 1.12)
        (when on-timeout (on-timeout))))))

(defn try-file-lock
  "Call f holding the OS lock on file when nobody else holds it; nil without calling f when someone does."
  [file f]
  (with-file-lock file 0 f))

(defn tmp-name [f] (str f ".tmp-" (.pid (java.lang.ProcessHandle/current)) "-" (System/nanoTime)))

(defn tmp? [f] (boolean (re-find #"\.tmp-\d+-\d+$" (str f))))

(defn spit-atomic
  "Write s to f whole or not at all: a temp file beside it, renamed over it."
  [f s]
  (fs/create-dirs (fs/parent f))
  (let [tmp (tmp-name f)]
    (spit tmp s)
    (fs/move tmp f {:replace-existing true :atomic-move true})))

;; --- the trace repo's key/value state: .git/trace-<ns>/<key>, edn (raw text for :prev) ------------

(defn clean-key [k] (clojure.string/replace (str k) #"[^A-Za-z0-9._-]" "_"))

(def ^:private raw-ns #{:prev})

(defn kv-path [repo ns k] (str (fs/path repo ".git" (str "trace-" (name ns)) (clean-key k))))

(defn kv-read
  "The value of k in ns of the trace repo at repo, or nil."
  [repo ns k]
  (let [f (kv-path repo ns k)]
    (when (fs/exists? f) (let [s (slurp f)] (if (raw-ns ns) s (clojure.edn/read-string s))))))

(defn kv-write!
  "Set k in ns of the trace repo at repo (whole or not at all); nil deletes."
  [repo ns k v]
  (let [f (kv-path repo ns k)]
    (if (nil? v) (fs/delete-if-exists f) (spit-atomic f (if (raw-ns ns) v (pr-str v))))))

(defn projects-lock
  "The lock around naming a new project (trace.observe/project-namer), shared by the hook and the store."
  [repo] (str (fs/path repo ".git" "trace-projects.lock")))

;; --- the spool: events the hook saw, waiting for the worker ---------------------------------------

(defn spool-dir [repo] (str (fs/path repo ".git" "trace-spool")))
(defn failed-dir [repo] (str (fs/path repo ".git" "trace-failed")))
(defn worker-lock [repo] (str (fs/path repo ".git" "trace-worker.lock")))

(defn entries
  "The spooled events in dir, oldest first (names are epoch nanos, then pid)."
  [dir]
  (if (fs/directory? dir)
    (sort (map str (filter #(and (clojure.string/ends-with? (str %) ".edn") (not (tmp? %))) (fs/list-dir dir))))
    []))

(defn entry-ms
  "The epoch ms an entry's name says it was spooled at."
  [f]
  (some-> (re-find #"^(\d+)-" (str (fs/file-name f))) second parse-long (quot 1000000)))
