(ns trace.copilot-events
  "Copilot sessions that no hook recorded, from Copilot's own log: bb copilot:catchup [SESSION-ID | events.jsonl]...
  (no argument: every session in the Copilot home). For a session run from IntelliJ (or the CLI) before
  `bb copilot:install` put the hooks in place, or with hooks that did not fire.

  The log (<copilot home>/session-state/<id>/events.jsonl, the same file for the CLI and the IntelliJ plugin)
  holds what the hooks would have been given: the prompt (hook.start userPromptSubmitted), every tool call
  (tool.execution_start + _complete), the end of a turn (agentStop) and of the session (sessionEnd). Each becomes
  the payload the hook would have had, goes through trace.copilot/adapt with the log as it was at that point
  (so model, tokens and time per call come out as they do live) and into the trace like a hook event
  (trace.hook/record!).

  Incremental: .git/trace-copilot-catchup/<session> in the trace repo remembers how far the log was taken, so a
  later run adds only what came after. No double recording: a session the hooks recorded is skipped (its state in
  the trace repo is newer than the catch-up's mark, or there is state and no mark at all).

  What it cannot give: the project tree after each call. The trace holds the tree of the worktree as it is
  WHEN THIS RUNS (observed like a hook does), so a session whose files have changed since shows its calls,
  prompts, outputs, replies, models and tokens exactly, but its diffs are the tree now against the tree at the
  last catch-up. Every payload carries `catchup: true` in its event note. Run it right after the session, or
  install the hooks first (bb copilot:install) and this is not needed."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [trace.copilot :as cop]
            [trace.git :as g]
            [trace.hook :as hook]))

(defn read-log [f]
  (vec (keep #(try (json/parse-string %) (catch Exception _ nil)) (str/split-lines (slurp (str f))))))

(defn payloads
  "[{:i index-in-log :payload hook-payload :es log-as-it-was}] of a parsed log, in order; f the log's path."
  [es f]
  (let [{:strs [data]} (first es)
        sid (get data "sessionId") cwd (get-in data ["context" "cwd"])
        base (fn [e event] {"hook_event_name" event "session_id" sid "cwd" cwd "timestamp" (get e "timestamp")})
        starts (into {} (keep #(when (= "tool.execution_start" (get % "type")) [(get-in % ["data" "toolCallId"]) (get % "data")]) es))]
    (vec
     (keep-indexed
      (fn [i e]
        (let [d (get e "data") t (get e "type")]
          (cond
            (and (= "hook.start" t) (= "userPromptSubmitted" (get d "hookType")))
            {:i i :es (subvec es 0 i) :payload (assoc (base e "UserPromptSubmit") "prompt" (get-in d ["input" "prompt"]))}

            (and (= "hook.start" t) (= "agentStop" (get d "hookType")))
            {:i i :es (subvec es 0 (inc i)) :payload (assoc (base e "Stop") "transcript_path" (str f) "stop_reason" (get-in d ["input" "stopReason"]))}

            (and (= "hook.start" t) (= "sessionEnd" (get d "hookType")))
            {:i i :es (subvec es 0 i) :payload (assoc (base e "SessionEnd") "reason" (get-in d ["input" "reason"]))}

            (= "tool.execution_complete" t)
            (when-let [s (get starts (get d "toolCallId"))]
              (let [ok? (not (false? (get d "success")))]
                {:i i :es (subvec es 0 i)
                 :payload (assoc (base e (if ok? "PostToolUse" "PostToolUseFailure"))
                                 "tool_name" (get s "toolName") "tool_input" (get s "arguments")
                                 "tool_result" {"result_type" (if ok? "success" "failure")
                                                "text_result_for_llm" (str (or (get-in d ["result" "content"]) (get-in d ["error" "message"]) (get d "error")))})}))
            :else nil)))
      es))))

;; --- recording ---------------------------------------------------------------------------

(defn- mark-file [repo sid] (fs/path repo ".git" "trace-copilot-catchup" sid))
(defn- state-file [repo sid] (fs/path repo ".git" "trace-sessions" sid))

(defn- mtime [f] (when (fs/exists? f) (.toMillis (fs/last-modified-time f))))

(defn hooked?
  "Did the hooks record this session (so a catch-up must leave it alone)?"
  [repo sid]
  (let [m (mtime (mark-file repo sid)) s (mtime (state-file repo sid))]
    (boolean (and s (or (nil? m) (> s (+ m 1000)))))))

(defn catch-up!
  "Record what the log f has that the trace has not. {:session :recorded n :skipped reason-or-nil}."
  [f & [{:keys [repo] :or {repo g/*repo*}}]]
  (let [es (read-log f) sid (get-in (first es) ["data" "sessionId"])]
    (cond
      (str/blank? (str sid)) {:session nil :recorded 0 :skipped "not a Copilot log"}
      (hooked? repo sid) {:session sid :recorded 0 :skipped "recorded by the hooks"}
      :else
      (let [mark (mark-file repo sid)
            done (or (some-> (when (fs/exists? mark) (slurp (str mark))) str/trim parse-long) -1)
            todo (filterv #(> (:i %) done) (payloads es f))]
        (doseq [{:keys [payload es]} todo]
          (when-let [[event input] (cop/adapt payload {:es es})]
            (hook/record! repo event (assoc input "catchup" true) (json/generate-string (assoc input "catchup" true)))))
        (when (seq todo)
          ((requiring-resolve 'trace.worker/drain!) (str repo) 120000)
          (fs/create-dirs (fs/parent mark))
          (spit (str mark) (str (dec (count es)))))
        {:session sid :recorded (count todo) :skipped nil}))))

(defn- logs [args]
  (let [home (str (cop/copilot-home))]
    (if (empty? args)
      (sort (map str (fs/glob (fs/path home "session-state") "*/events.jsonl")))
      (map (fn [a] (if (fs/regular-file? a) a (str (fs/path home "session-state" a "events.jsonl")))) args))))

(defn -main [& args]
  (doseq [f (logs args)]
    (let [{:keys [session recorded skipped]} (try (catch-up! f) (catch Throwable e {:skipped (str "failed: " (ex-message e))}))]
      (println (subs (str session "--------") 0 8) (if skipped (str "skipped, " skipped) (str recorded " events recorded")))))
  (System/exit 0))
