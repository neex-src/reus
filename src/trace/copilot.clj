(ns trace.copilot
  "GitHub Copilot CLI sessions into the same trace as the Claude ones. Copilot calls its hooks with the
  VS Code / Claude style payload (PascalCase event names switch it on: snake_case fields, Claude's tool
  names), so this is only an adapter in front of trace.hook:

    bb --config <reus>/bb.edn -m trace.copilot        (payload on stdin; the event is its hook_event_name)

  Subscribed events (copilot-lab/config/settings.json, `hooks`): UserPromptSubmit -> prompt,
  PostToolUse / PostToolUseFailure -> tool, Stop / SessionEnd -> stop. SessionStart says nothing the
  others do not, and is ignored.

  What the adapter changes, seen in real Copilot 1.0.105 payloads (test/copilot/*.jsonl):
    - tool_input: Copilot's keys become Claude's (path -> file_path, old_str -> old_string, new_str ->
      new_string, file_text -> content), so observe finds the file a call is about;
    - tool_result.text_result_for_llm becomes tool_response; the shell's tail `<shellId: N completed
      with exit code K>` goes, and a K other than 0 becomes the `Exit code K` head Claude's results have;
      a result_type other than success marks the response as an error;
    - transcript_path is dropped (Stop sends Copilot's own events.jsonl, not a Claude transcript); the
      reply of a Stop, its model and its token counts are read from that file instead
      (<copilot home>/session-state/<session>/events.jsonl), and handed over as last_assistant_message,
      model and usage, the way trace.record takes them from a payload without a transcript;
    - the original payload rides along as \"copilot\", so refs/notes/event keeps every field.

  The IntelliJ plugin's Copilot agent runs the same engine and calls the same hook files with the same payloads
 . What differs: its session log says `client_name: copilot-intellij` in workspace.yaml
  (kept as `client` in the event), its model may be `auto` (then the model the auto mode chose is the session's),
  and GPT models' apply_patch has one string for arguments (tool-input). trace.copilot-events replays a finished log.

  Nothing else is touched: observe, spool, worker, record and the gui see a Claude-shaped event. Nothing
  in the Claude path requires this ns. Like trace.hook, it always exits 0."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [trace.git :as g]
            [trace.health :as health]
            [trace.hook :as hook]))

(def ^:private events
  {"UserPromptSubmit" "prompt" "PostToolUse" "tool" "PostToolUseFailure" "tool" "Stop" "stop" "SessionEnd" "stop"})

(def ^:private claude-tools
  {"bash" "Bash" "powershell" "Bash" "view" "Read" "create" "Write" "edit" "Edit" "str_replace_editor" "Edit"
   "apply_patch" "Edit" "grep" "Grep" "rg" "Grep" "glob" "Glob" "web_fetch" "WebFetch" "web_search" "WebSearch"
   "ask_user" "AskUserQuestion" "update_todo" "TodoWrite" "task" "Agent"})

(def ^:private claude-keys
  {"path" "file_path" "old_str" "old_string" "new_str" "new_string" "file_text" "content"})

(defn- patch-file
  "The first file an apply_patch text (`*** Add File: p`, `*** Update File: p`, `*** Delete File: p`) is about."
  [s]
  (some-> (re-find #"(?m)^\*\*\* (?:Add|Update|Delete) File: (.+?)\s*$" s) second))

(defn tool-input
  "Claude-shaped tool input. A tool whose arguments are one string (the GPT models' apply_patch) becomes
  {input text, file_path the patch's first file}."
  [m]
  (cond (map? m) (update-keys m #(get claude-keys % %))
        (string? m) (cond-> {"input" m} (patch-file m) (assoc "file_path" (patch-file m)))
        :else {}))

(defn tool-response
  "Claude-shaped result text of a Copilot tool_result: the shell tail turned into an `Exit code K` head
  for a K other than 0; a result that did not succeed is an error."
  [{:strs [result_type text_result_for_llm]}]
  (let [t (str text_result_for_llm)
        [_ body k] (re-matches #"(?s)(.*?)\s*<shellId: \S+ completed with exit code (-?\d+)>\s*" t)
        t (if body (if (= "0" k) body (str "Exit code " k "\n" body)) t)]
    (if (and result_type (not= "success" result_type)) {"text" t "is_error" true} t)))

;; --- Copilot's own log of the session ------------------------------------------------------

(defn copilot-home [] (or (System/getenv "COPILOT_HOME") (str (fs/path (fs/home) ".copilot"))))

(defn- events-file [{:strs [session_id transcript_path]}]
  (let [f (or transcript_path (str (fs/path (copilot-home) "session-state" session_id "events.jsonl")))]
    (when (fs/regular-file? f) (str f))))

(defn- read-events [f]
  (when f (vec (keep #(try (json/parse-string %) (catch Exception _ nil)) (str/split-lines (slurp f))))))

(defn- this-turn
  "The events after the last user.message."
  [es]
  (let [i (or (last (keep-indexed #(when (= "user.message" (get %2 "type")) %1) es)) -1)]
    (subvec es (inc i))))

(defn session-model
  "The model the session runs: the first event (session.start) says it; for `auto` (the IntelliJ plugin's
  default) the model Copilot's auto mode chose (session.auto_mode_resolved, among the first events)."
  [f]
  (when f
    (with-open [r (clojure.java.io/reader f)]
      (let [es (keep #(try (json/parse-string %) (catch Exception _ nil)) (take 12 (line-seq r)))
            m (some-> (first es) (get-in ["data" "selectedModel"]))]
        (if (= "auto" m)
          (or (some #(when (= "session.auto_mode_resolved" (get % "type")) (get-in % ["data" "chosenModel"])) es) m)
          m)))))

(defn client-name
  "The client that runs the session (workspace.yaml's client_name beside the log): `github/cli` for the CLI,
  `copilot-intellij` for the IntelliJ plugin; nil when unknown."
  [f]
  (when f
    (let [w (fs/path (fs/parent f) "workspace.yaml")]
      (when (fs/regular-file? w)
        (some-> (re-find #"(?m)^client_name:\s*(\S+)" (slurp (str w))) second)))))

(defn usage-of
  "Claude's usage map (input_tokens = the uncached part; Copilot's inputTokens counts the cached too) of
  Copilot usage records, summed; nil for none."
  [us]
  (when (seq us)
    (let [sum (fn [k] (reduce + 0 (keep #(get % k) us)))
          read (sum "cacheReadTokens") write (sum "cacheWriteTokens")]
      {:input_tokens (max 0 (- (sum "inputTokens") read write)) :output_tokens (sum "outputTokens")
       :cache_read_input_tokens read :cache_creation_input_tokens write
       :reasoning_tokens (sum "reasoningTokens") :copilot_cost (sum "cost")})))

(defn turn-facts
  "{:reply :model :usage} of the last prompt's events: the last assistant text, the model that wrote it,
  the tokens of all its model calls (the transcript's key names)."
  [es]
  (let [t (this-turn es)
        said (last (filter #(and (= "assistant.message" (get % "type")) (not (str/blank? (get-in % ["data" "content"])))) t))
        us (keep #(when (= "session.usage_record" (get % "type")) (get-in % ["data" "usage"])) t)]
    {:reply (get-in said ["data" "content"])
     ;; the prompt is over when its last model message asks for no tool; a log read before Copilot flushed that
     ;; message ends at an earlier one (which has tool requests, or is the text before them)
     :final? (let [m (last (filter #(= "assistant.message" (get % "type")) t))]
               (boolean (and m (empty? (get-in m ["data" "toolRequests"])))))
     :model (or (get-in said ["data" "model"]) (some #(get % "model") us))
     :usage (usage-of us)}))

(defn- claude-name [n] (get claude-tools n n))

(defn- millis [ts] (try (.toEpochMilli (java.time.Instant/parse ts)) (catch Exception _ nil)))

(defn call-facts
  "{:model :usage :duration-ms} of the tool call a PostToolUse hook is about, from the log: the model request
  that asked for it is the earliest one in this prompt, for that tool and those arguments, whose call has not
  completed yet (Copilot fires the hook before it writes tool.execution_complete). The tokens of a request go
  to the first of its calls only (as with Claude's transcripts: the sum over the calls stays the request's).
  Nil when the log does not show it."
  [es tool-name args now-ms]
  (let [t (this-turn es)
        done (set (keep #(when (= "tool.execution_complete" (get % "type")) (get-in % ["data" "toolCallId"])) t))
        reqs (for [m t :when (= "assistant.message" (get m "type"))
                   [i r] (map-indexed vector (get-in m ["data" "toolRequests"]))
                   :when (and (not (done (get r "toolCallId"))) (= tool-name (claude-name (get r "name"))))]
               {:m m :i i :r r})
        {:keys [m i r]} (or (first (filter #(= args (get-in % [:r "arguments"])) reqs)) (first reqs))]
    (when r
      (let [api (get-in m ["data" "apiCallId"])
            started (some #(when (and (= "tool.execution_start" (get % "type")) (= (get r "toolCallId") (get-in % ["data" "toolCallId"]))) (millis (get % "timestamp"))) t)
            us (when (zero? i) (filter #(= api (get % "apiCallId")) (keep #(when (= "session.usage_record" (get % "type")) (get-in % ["data" "usage"])) t)))]
        (cond-> {:model (get-in m ["data" "model"])}
          (seq us) (assoc :usage (usage-of us))
          started (assoc :duration-ms (max 0 (- now-ms started))))))))

(defn- stop-facts
  "turn-facts of the session's log; Copilot writes the answer before it fires Stop, but the log may not have it on disk yet: wait (2 s at most) for the prompt's last message."
  [in]
  (loop [n 0]
    (let [f (events-file in)
          r (some-> f read-events turn-facts)]
      (if (or (:final? r) (>= n 10))
        (assoc r :file f)
        (do (Thread/sleep 200) (recur (inc n)))))))

;; --- the adapter ------------------------------------------------------------------------

(defn adapt
  "[event claude-shaped-input] for a Copilot hook payload (parsed), nil for an event that is not recorded. `es`: the
  session's events as they were when the hook ran (trace.copilot-events replays a finished log); default: the log on disk."
  [in & [{:keys [es]}]]
  (when-let [event (events (get in "hook_event_name"))]
    (let [{:strs [session_id cwd hook_event_name tool_name tool_input tool_result error prompt]} in
          log (events-file in)
          model (session-model log)
          client (client-name log)
          base (cond-> {"session_id" session_id "cwd" cwd "hook_event_name" hook_event_name "copilot" in}
                 client (assoc "client" client)
                 (str/blank? (str session_id)) (dissoc "session_id"))]
      [event
       (case event
         "prompt" (cond-> (assoc base "prompt" prompt) model (assoc "model" model))
         "tool" (let [{cm :model :keys [usage duration-ms]} (some-> (or es (some-> (events-file in) read-events))
                                                                       (call-facts (claude-name tool_name) tool_input (or (millis (str (get in "timestamp"))) (System/currentTimeMillis))))]
                  (cond-> (assoc base
                                 "tool_name" (claude-name tool_name)
                                 "tool_input" (tool-input tool_input)
                                 "tool_response" (tool-response tool_result))
                    error (assoc "error" error)
                    (or cm model) (assoc "model" (or cm model))
                    usage (assoc "usage" usage)
                    duration-ms (assoc "duration_ms" duration-ms)))
         "stop" (let [{:keys [reply usage] smodel :model} (when (= "Stop" hook_event_name) (if es (turn-facts es) (stop-facts in)))]
                  (cond-> base
                    reply (assoc "last_assistant_message" reply)
                    (or smodel model) (assoc "model" (or smodel model))
                    usage (assoc "usage" usage))))])))

(defn -main [& _]
  (let [raw (slurp *in*)
        in (try (json/parse-string raw) (catch Exception _ {}))]
    (try
      (when-let [[event input] (adapt in)]
        (hook/record! g/*repo* event input (json/generate-string input)))
      (catch Throwable e
        (try (health/log! "copilot hook" (str (get in "hook_event_name")) in e)
             (health/fail! :hook (str (get in "hook_event_name")) in e)
             (catch Throwable _))))
    (System/exit 0)))
