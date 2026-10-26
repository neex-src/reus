(ns trace.hook
  "The Claude Code hook, global, for every session on the machine:

    bb --config <reus>/bb.edn -m trace.hook <event>
      prompt          UserPromptSubmit
      tool            PostToolUse / PostToolUseFailure
      stop            Stop, SessionEnd
      subagent-start  SubagentStart
      subagent-stop   SubagentStop

  Claude waits for it, so it does what must happen before Claude goes on and nothing more: parse the
  payload, observe the event (trace.observe: the worktree it acted in, hashed into the trace repo -
  the one thing that cannot wait, the next call may change it), write the event and its observation
  to the spool (.git/trace-spool/<epoch nanos>-<pid>.edn, whole or not at all) and wake the worker
  (trace.worker) when none is draining. The worker makes the history from the spool.

  Nothing it does takes a lock but the one of the worktree's own index (other sessions in other
  worktrees never wait for it). A failure is logged (.git/trace-errors.log, trace.health :hook) and
  the event is spooled anyway with what could be seen; the hook always exits 0, so tracing never
  breaks a session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [trace.disk :as disk]
            [trace.files :as files]
            [trace.git :as g]
            [trace.health :as health]
            [trace.observe :as observe]))

(defn- tkey [{:strs [agent_id session_id]}] (if agent_id (str "agent-" agent_id) session_id))

(defn observe!
  "The observation of an event; the worktree a session (or agent) last wrote in is kept for its next
  prompt and its reads."
  [repo event input]
  (let [key (tkey input)
        namer (observe/project-namer #(files/kv-read repo :projects %) #(files/kv-write! repo :projects %1 %2))
        name-of (fn [p] (files/with-file-lock (files/projects-lock repo) 5000 #(namer p) #(namer p)))
        obs (observe/observe (disk/live repo) event input {:last-root (files/kv-read repo :hookroot key) :name-of name-of})]
    (when-let [r (and (#{"tool" "prompt"} event) (not (:read? obs)) (get-in obs [:wt :root]))]
      (when (not= r (files/kv-read repo :hookroot key)) (files/kv-write! repo :hookroot key r)))
    obs))

(defn- epoch-nanos []
  (let [i (java.time.Instant/now)] (+ (* (.getEpochSecond i) 1000000000) (.getNano i))))

(defn spool!
  "Write an entry {:event :raw :obs} to the spool; its path."
  [repo entry]
  (let [f (str (fs/path (files/spool-dir repo) (format "%019d-%d.edn" (epoch-nanos) (.pid (java.lang.ProcessHandle/current)))))]
    (files/spit-atomic f (pr-str entry))
    f))

(defn wake!
  "Start a worker for repo unless one is draining it: detached (its own session, stdio to its log),
  so neither this hook nor Claude Code waits for it."
  [repo]
  (when (files/try-file-lock (files/worker-lock repo) (constantly true))
    (let [log (io/file (str (fs/path repo ".git" "trace-worker.log")))]
      (p/process {:in "" :out :append :out-file log :err :append :err-file log
                  :extra-env {"TRACE_REPO" (str repo)}}
                 "setsid" "-f" "bb" "--config" (str (fs/path g/home "bb.edn")) "-m" "trace.worker"))))

(defn ensure-repo!
  "The trace repo, made when it is not there."
  [repo]
  (when-not (fs/exists? (fs/path repo ".git"))
    (fs/create-dirs repo)
    (p/shell {:dir (str repo) :out :string :err :string} "git" "init" "-q" "-b" "_")))

(defn- record-event! [repo event input raw]
  (let [obs (try (observe! repo event input)
                 (catch Throwable e
                   (health/log! "hook" event input e)
                   (health/fail! :hook event input e)
                   {:at (System/currentTimeMillis) :error (or (ex-message e) (str e))}))
        root (get-in obs [:wt :root])]
    (spool! repo {:event event :raw raw :obs obs})
    (cond (:snap-error obs) (let [e (ex-info (:snap-error obs) {})]
                              (health/log! "hook" event input e)
                              (health/fail! :hook event input e root))
          (:error obs) nil
          :else (health/ok! :hook root))
    (wake! repo)))

(defn record!
  "Observe, spool and wake: what one hook call does with its event. A payload without a session
  (not JSON, or not Claude Code's) is no event: logged, not spooled."
  [repo event input raw]
  (ensure-repo! repo)
  (if (str/blank? (str (get input "session_id")))
    (health/log! "hook" event input (ex-info (str "no session_id in the payload: " (subs (str raw) 0 (min 120 (count (str raw))))) {}))
    (record-event! repo event input raw)))

(defn -main [& [event]]
  (let [raw (slurp *in*)
        input (try (json/parse-string raw) (catch Exception _ {}))]
    (try
      (record! g/*repo* event input raw)
      (catch Throwable e
        (health/log! "hook" event input e)
        (try (health/fail! :hook event input e) (catch Throwable _))))
    (System/exit 0)))
