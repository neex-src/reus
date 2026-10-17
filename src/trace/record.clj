(ns trace.record
  "Global Claude Code hook: every session on this machine becomes git history in one
  trace repo (trace.git/*repo*), nothing squashed.

  Hook command:  bb --config <reus>/bb.edn -m trace.record <event>
    prompt          UserPromptSubmit
    tool            PostToolUse / PostToolUseFailure
    stop            Stop, SessionEnd
    subagent-start  SubagentStart
    subagent-stop   SubagentStop

  Layout. The project is the session's repo (the main checkout's dir name, also for
  a worktree of it); outside git, the ~/projects/<name> it sits in; elsewhere nothing
  is traced. Branch <project> gets one --no-ff merge per prompt; prompt NN is the
  branch <project>-pNN, cut from <project>'s tip, numbered across all of the
  project's sessions:
    1. `prompt NN: ...`, doto.md = the prompt; body: session, cwd, root
    2. one commit per tool call, in the order the hooks arrive:
       * an edit tool on an existing file: the file is copied to files/<rel>; the
         first time a file is seen a `baseline <rel>` commit holds it as it was
         before the edit (reversed from the Edit input, else the project's HEAD,
         else empty), so the diff of the edit commit is the edit
       * a read of one file (a read tool, or a command naming exactly one
         existing file of the project): the file goes into the repo as a blob,
         its sha is the subject, the output is reads/<rel>
       * anything else: subject NN.k, output in reads/_cmd/NN.k
       The body is the command or `<tool> <input json>`; a failure is appended
       after `--- error ---` (or is the output, after that line).
    3. `response NN`, doto.md = the reply; then the merge into <project>.
  <rel> is relative to the session's checkout root; a file outside it is abs/<path>.

  A subagent run is recorded like a session of its own: SubagentStart opens the branch
  <project>-pNN-a<agent id>, cut from the tip of the parent's prompt NN at that moment
  (its open prompt, else its last); its first commit is `prompt NN-a<id>: ...` with the
  agent's brief; every tool event that carries an agent_id goes to that branch, never to
  the parent's; SubagentStop adds `response NN-a<id>`. The branch stays a side line until
  the parent reacts to the agent's notification: the project merge of that prompt then gets
  the agent's tip as a further parent (the prompt open when the agent stops, else the prompt
  opened by the `<task-notification>` naming the agent). An agent nobody reacted to stays a
  dangling branch. SessionEnd closes agents still open.

  Every event's hook stdin is kept verbatim as a git note (refs/notes/event) on the commit
  it made, so nothing of the payload is dropped. Beside it, refs/notes/probe says whether the
  session transcript already held the event when the hook fired (trace.probe; live only).
  refs/notes/turn holds what the transcript adds to the event, the model's side (trace.turn: model,
  usage, thinking, durations; the turn's usage at its end), also live from the tail of the
  file; refs/notes/output the output file of a background task a prompt's notification names.
  A tool output Claude Code persisted to a file is recorded in full.

  Nothing is ever checked out: commits are built with a private index and
  commit-tree and moved with update-ref, under one mkdir lock, so concurrent
  sessions cannot disturb each other. The open prompt of a session is
  .git/trace-sessions/<session id>. Errors go to .git/trace-errors.log; the hook
  always exits 0, so tracing never breaks a session.

  trace.replay feeds old transcripts through the same handlers, with a batch store and the
  disk as it was then (trace.shadow): every read or write of the trace repo goes through a
  trace.store, every look at the machine through a trace.disk."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [trace.agents :as agents]
            [trace.disk :as disk]
            [trace.git :as g]
            [trace.probe :as probe]
            [trace.turn :as turn]
            [trace.store :as store]))

(def delim "--- error ---")

(def edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})
(def read-tools #{"Read" "NotebookRead" "read_file"})

(def ^:dynamic *probe?* "Measure the transcript at hook time (the live hook; not a replay)." false)

(def notes-ref "refs/notes/event")
(def probe-ref "refs/notes/probe")
(def turn-ref "refs/notes/turn")
(def output-ref "refs/notes/output")

;; x is the context of one event: {:s store :d disk :raw payload as received :probed probe}

(defn- gitdir [& more] (str (apply fs/path g/*repo* ".git" more)))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "...") s)))
(defn- first-line [s n] (clip (first (str/split-lines (str s))) n))
(defn- short-tool [t] (str/replace (str t) #"^mcp__.*__" ""))

;; --- the store, by branch -----------------------------------------------------------

(defn ensure-repo! [] (store/ensure! (store/live g/*repo*)))

(defn- tip [x ref] (store/tip (:s x) (if (str/starts-with? ref "refs/") ref (str "refs/heads/" ref))))
(defn- advance! [x branch sha] (store/advance! (:s x) (str "refs/heads/" branch) sha))
(defn- has? [x rev path] (store/has? (:s x) rev path))

(defn- commit!
  "A commit on top of parent whose tree is parent's plus files {path content};
  merge-parent makes it a merge. Returns its id."
  [x parent files subject body & [merge-parent]]
  (store/commit! (:s x) {:parents (vec (remove nil? [parent merge-parent])) :base parent
                         :files files :subject subject :body body}))

(defn- note!
  "Keep the event's raw payload (and its probe) as notes on commit sha."
  [x sha]
  (when (:raw x)
    (store/note! (:s x) notes-ref sha (:raw x))
    (when (:probed x) (store/note! (:s x) probe-ref sha (pr-str (:probed x))))
    (when-let [t (:turn x)]
      (store/note! (:s x) turn-ref sha (pr-str (dissoc t :pending-id)))
      (when-let [id (:pending-id t)] ; the tool_use was not in the transcript yet: looked for at the next hook
        (store/kv! (:s x) :turn-pending (:tkey x) {:id id :commit sha})))))

;; --- sessions -----------------------------------------------------------------

(defn- state [x sid] (store/kv (:s x) :sessions sid))
(defn- state! [x sid m] (store/kv! (:s x) :sessions sid m))

(defn- last-prompt
  "The session's most recent prompt {:project :branch :nn :root}, open or not."
  [x sid] (store/kv (:s x) :last sid))

(defn- agent-key [agent-id] (str "agent-" agent-id))

(defn- pending!
  "A finished agent whose parent was idle: its tip waits for the prompt that reacts to its notification."
  [x agent-id m] (store/kv! (:s x) :pending agent-id m))
(defn- take-pending! [x agent-id]
  (when-let [m (store/kv (:s x) :pending agent-id)]
    (store/kv! (:s x) :pending agent-id nil)
    (:tip m)))

(defn project-of
  "{:project name :root dir} for a session cwd, or nil when it is not traced."
  ([cwd] (project-of (disk/live g/*repo*) cwd))
  ([d cwd] (disk/project-of d cwd)))

(defn- next-n [x project]
  (let [names (store/refs (:s x) (str "refs/heads/" project "-p"))]
    (inc (reduce max -1 (keep #(some-> (re-find #"-p(\d+)$" %) second parse-long) names)))))

;; --- what a tool call did -------------------------------------------------------

(defn- text-of [r]
  (cond
    (nil? r) ""
    (string? r) r
    (sequential? r) (str/join "\n" (map text-of r))
    (map? r) (let [parts (keep #(let [v (get r %)] (when-not (nil? v) (text-of v)))
                               ["stdout" "stderr" "text" "content" "output" "result" "file" "error"])]
               (if (seq parts) (str/join "\n" (remove str/blank? parts)) (json/generate-string r)))
    :else (str r)))

(defn- failed? [{:strs [hook_event_name tool_response]} text]
  (or (= hook_event_name "PostToolUseFailure")
      (and (map? tool_response) (true? (get tool_response "is_error")))
      (boolean (when-let [[_ n] (re-find #"(?i)exit code:?\s*(\d+)" (subs text 0 (min 80 (count text))))]
                 (not= n "0")))))

(defn- command-of [{:strs [tool_name tool_input]}]
  (or (get tool_input "command") (get tool_input "code")
      (str tool_name " " (json/generate-string tool_input))))

(defn- rel-of [root f]
  (let [c (str f)]
    (if (str/starts-with? c (str root "/")) (subs c (inc (count root))) (str "abs" c))))

(defn- the-file
  "The one existing regular file this call is about: its file_path/path/notebook_path,
  else the one existing file of the root a shell command names. Canonical, as the
  session knew it."
  [x root {:strs [tool_input cwd]}]
  (let [resolve (fn [tok]
                  (disk/resolve-file (:d x) (str/replace (str tok) #"^['\"]|['\"]$" "") [(or cwd root) root]))]
    (if-let [fp (some #(get tool_input %) ["file_path" "path" "notebook_path"])]
      (resolve fp)
      (when-let [cmd (get tool_input "command")]
        (let [fs* (distinct (filter #(str/starts-with? % (str root "/"))
                                    (keep resolve (str/split cmd #"[\s;|&<>()]+"))))]
          (when (= 1 (count fs*)) (first fs*)))))))

(defn- before-edit
  "The file as it was before this edit, as well as can be told after it."
  [x root rel now {:strs [tool_name tool_input]}]
  (let [undo (fn [s {:strs [old_string new_string replace_all]}]
               (cond (or (nil? new_string) (nil? old_string)) s
                     replace_all (str/replace s new_string old_string)
                     :else (str/replace-first s new_string old_string)))]
    (case (short-tool tool_name)
      "Edit" (undo now tool_input)
      "MultiEdit" (reduce undo now (reverse (get tool_input "edits")))
      (disk/head-content (:d x) root rel))))

;; --- events -----------------------------------------------------------------------

(defn- reply-of [{:strs [last_assistant_message transcript_path]}]
  (or (when-not (str/blank? last_assistant_message) last_assistant_message)
      (when (and transcript_path (fs/exists? transcript_path))
        (->> (str/split-lines (slurp transcript_path))
             (keep #(let [m (try (json/parse-string %) (catch Exception _ nil))]
                      (when (= "assistant" (get m "type"))
                        (let [c (get-in m ["message" "content"])]
                          (when (sequential? c)
                            (str/join "\n" (keep (fn [b] (when (= "text" (get b "type")) (get b "text"))) c)))))))
             (remove str/blank?)
             last))
      ""))

(defn- full-output
  "The output of a tool call: when Claude Code kept the whole of it in a file and the result is
  a preview that names it, the file's content."
  [x in text]
  (let [p (or (get-in in ["tool_response" "persistedOutputPath"])
              (second (re-find #"Full output saved to: (\S+\.txt)" text)))]
    (or (some->> p (disk/external-text (:d x))) text)))

(defn- close!
  "Reply to the open prompt of key (a session id, or an agent's key) and merge it into
  its project (--no-ff: the merge's tree is the prompt's). An agent's branch is not merged."
  [x key reply & [note?]]
  (when-let [{:keys [project branch nn subject agent merge]} (state x key)]
    (when-let [t (tip x branch)]
      (let [r (commit! x t {"doto.md" reply} (str "response " nn) "")]
        (advance! x branch r)
        (when note? (note! x r))
        (when-not agent
          ;; finished agents whose notification this prompt reacted to are merged with it
          (advance! x project (store/commit! (:s x) {:parents (into [(tip x project) r] merge) :base r
                                                     :files {} :subject subject})))))
    (state! x key nil)))

(defn on-prompt [x {:strs [session_id prompt cwd]}]
  (close! x session_id "")
  (when-let [{:keys [project root]} (disk/project-of (:d x) cwd)]
    (when-not (tip x project)
      (advance! x project (commit! x nil {} (str "trace of " project) (str "root " root))))
    (let [nn (format "%02d" (next-n x project))
          branch (str project "-p" nn)
          subject (str "prompt " nn ": " (clip prompt 100))
          c (commit! x (tip x project) {"doto.md" prompt} subject
                     (str "session " session_id "\ncwd " cwd "\nroot " root))]
      (advance! x branch c)
      (note! x c)
      (when-let [out (some->> (re-find #"<output-file>(.+?)</output-file>" (str prompt)) second str/trim (disk/external-text (:d x)))]
        (store/note! (:s x) output-ref c out))
      (store/kv! (:s x) :last session_id {:project project :branch branch :nn nn :root root})
      (state! x session_id {:project project :branch branch :nn nn :root root :k 0 :subject subject
                            :merge (vec (keep #(take-pending! x %) (map second (re-seq #"<task-id>([A-Za-z0-9]+)</task-id>" (str prompt)))))}))))

(defn on-tool [x {:strs [session_id agent_id tool_name tool_input hook_event_name] :as in}]
  (let [key (if agent_id (agent-key agent_id) session_id)]
   (when-let [{:keys [branch nn root k] :as st} (state x key)]
    (when-let [t (tip x branch)]
      (let [k (inc k)
            tool (short-tool tool_name)
            cmd (command-of in)
            text (text-of (get in "tool_response"))
            text (if (= hook_event_name "PostToolUseFailure") (or (get in "error") text) text)
            bad? (failed? in text)
            text (full-output x in text)
            file (the-file x root in)
            rel (some->> file (rel-of root))
            agent (when-let [a (get in "agent_type")] (str "\n\n" "agent " a))
            bytes-of-file #(or (disk/file-bytes (:d x) file) (byte-array 0))
            c (cond
                (and file (edit-tools tool) (not bad?))
                (let [now (String. ^bytes (bytes-of-file) "UTF-8")
                      path (str "files/" rel)
                      t (if (has? x t path)
                          t
                          (commit! x t {path (before-edit x root rel now in)} (str "baseline " rel)
                                   "the file before its first traced edit"))]
                  (commit! x t {path now} (str tool ": " (first-line rel 90)) (str cmd agent)))

                (and file (or (read-tools tool) (get tool_input "command")))
                (commit! x t {(str "reads/" rel) (if bad? (str delim "\n" text) text)}
                         (store/blob! (:s x) (bytes-of-file)) (str cmd agent))

                :else
                (commit! x t {(format "reads/_cmd/%s.%d" nn k) (if bad? (str delim "\n" text) text)}
                         (format "%s.%d" nn k) (str cmd agent)))]
        (advance! x branch c)
        (note! x c)
        (state! x key (assoc st :k k)))))))

;; --- subagents ----------------------------------------------------------------------

(defn- agent-brief
  "The brief a subagent was given: the first message of its transcript (written as it starts)."
  [session-id agent-id]
  (loop [n 0]
    (if-let [f (agents/find-file session-id agent-id ".jsonl")]
      (or (with-open [r (io/reader (str f))]
            (some-> (first (line-seq r)) (json/parse-string true) :message :content
                    (#(if (string? %) % (str/join "\n" (keep :text %))))))
          "")
      (if (< n 10) (do (Thread/sleep 100) (recur (inc n))) ""))))

(defn on-subagent-start [x {:strs [session_id agent_id agent_type cwd]}]
  (when-let [{:keys [project branch nn root]} (or (state x session_id) (last-prompt x session_id))]
    (when-let [t (and agent_id (tip x branch))]
      (let [anid (str nn "-a" agent_id)
            ab (str project "-p" anid)
            brief (agent-brief session_id agent_id)
            subject (str "prompt " anid ": " (clip (str agent_type ": " (first-line brief 80)) 100))
            c (commit! x t {"doto.md" brief} subject
                       (str "session " session_id "\ncwd " cwd "\nroot " root "\nagent " agent_type
                            "\nagent_id " agent_id "\nparent " branch))]
        (advance! x ab c)
        (note! x c)
        (state! x (agent-key agent_id) {:project project :branch ab :nn anid :root root :k 0 :subject subject
                                      :agent true :session session_id})))))

(defn- agent-reply
  "What an agent said last: the payload's message, else the last assistant text of its transcript."
  [session-id agent-id message]
  (or (when-not (str/blank? message) message)
      (when-let [f (agents/find-file session-id agent-id ".jsonl")]
        (turn/last-text (turn/read-entries (:lines (probe/tail-lines f (* 256 1024))))))
      ""))

(defn on-subagent-stop
  "Close the agent's branch. Its tip is merged where the parent reacts to the agent's
  notification: into the parent's prompt when one is open (the notification is delivered
  into that turn), else into the prompt that is opened by the notification."
  [x {:strs [session_id agent_id last_assistant_message]}]
  (when agent_id
    (let [b (:branch (state x (agent-key agent_id)))]
      (close! x (agent-key agent_id) (agent-reply session_id agent_id last_assistant_message) true)
      (when-let [t (and b (tip x b))]
        (if-let [st (state x session_id)]
          (state! x session_id (update st :merge (fnil conj []) t))
          (pending! x agent_id {:tip t :session session_id}))))))

(defn- agent-states-of [x session-id]
  (keep (fn [[k st]]
          (when (and (:agent st) (= session-id (:session st))) (str/replace k #"^agent-" "")))
        (store/kv-all (:s x) :sessions)))

(defn on-stop [x {:strs [session_id hook_event_name] :as in}]
  (close! x session_id (reply-of in) true)
  (when (= "SessionEnd" hook_event_name)
    (doseq [a (agent-states-of x session_id)] (close! x (agent-key a) "" false))))

(defn- probe-event
  "The probe of an event to be recorded. A tool event also gets the previous tool call of its
  transcript looked up, and becomes the previous one."
  [x event {:strs [session_id agent_id tool_use_id] :as input}]
  (if (= event "tool")
    (let [k (if agent_id (agent-key agent_id) session_id)
          m (probe/measure event input (store/kv (:s x) :prev k))]
      (when-not (str/blank? tool_use_id) (store/kv! (:s x) :prev k tool_use_id))
      m)
    (probe/measure event input)))

(defn- transcript-of [{:strs [transcript_path agent_id session_id]}]
  (if agent_id (some-> (agents/find-file session_id agent_id ".jsonl") str) transcript_path))

(defn- tail-index
  "The index of the tail of a transcript (the last `window` bytes)."
  [file window]
  (turn/index (turn/read-entries (:lines (probe/tail-lines file window)))))

(def ^:private windows [(* 256 1024) (* 8 1024 1024)])

(defn- live-turn
  "What the transcript adds to a live event (trace.turn), read from its tail; nil when it is not
  there. A tool call whose tool_use is not there yet is {:pending-id id}, found at the next hook."
  [event {:strs [tool_use_id prompt] :as input}]
  (try
    (let [file (transcript-of input)]
      (when (and file (fs/exists? file))
        (case event
          "tool" (or (some #(turn/tool (tail-index file %) tool_use_id {:duration-ms (get input "duration_ms")}) windows)
                     {:use-found? false :pending-id tool_use_id})
          "prompt" (let [ix (tail-index file (first windows))]
                     (some->> (turn/find-prompt ix prompt) (turn/prompt ix)))
          "stop" (turn/stop (tail-index file (first windows)))
          nil)))
    (catch Throwable _ nil)))

(defn- late-turn
  "The turn data of the previous tool call of this key, when it was not in the transcript then."
  [x key input]
  (try
    (when-let [{:keys [id commit]} (store/kv (:s x) :turn-pending key)]
      (when-let [file (transcript-of input)]
        (when-let [t (some #(turn/tool (tail-index file %) id {}) windows)]
          {:commit commit :turn (assoc t :late? true)})))
    (catch Throwable _ nil)))

(defn- recorded?
  "Will this event make a commit? Read-only and cheap, so the probe (which may wait) is only paid for those."
  [x event {:strs [session_id agent_id cwd]}]
  (boolean
   (case event
     "tool" (state x (if agent_id (agent-key agent_id) session_id))
     "prompt" (disk/project-of (:d x) cwd)
     "stop" (state x session_id)
     "subagent-start" (or (state x session_id) (last-prompt x session_id))
     "subagent-stop" (and agent_id (state x (agent-key agent_id)))
     nil)))

(defn handle-in!
  "Handle one hook event in context x ({:s store :d disk :probe? bool}); raw is its stdin as
  received (default: the parsed input re-encoded)."
  [x event input & [raw]]
  (store/ensure! (:s x))
  (let [x (assoc x :raw (or raw (json/generate-string input)))
        live? (and (:probe? x) (recorded? x event input))
        x (assoc x :probed (when live? (probe-event x event input))
                 :turn (if live? (live-turn event input) (:turn x))
                 :tkey (if (get input "agent_id") (agent-key (get input "agent_id")) (get input "session_id")))
        late (when (and live? (#{"tool" "stop"} event)) (late-turn x (:tkey x) input))]
    (store/locked
     (:s x)
     #(case (do (when late
                  (store/note! (:s x) turn-ref (:commit late) (pr-str (:turn late)))
                  (store/kv! (:s x) :turn-pending (:tkey x) nil))
                event)
        "prompt" (on-prompt x input)
        "tool" (on-tool x input)
        "stop" (on-stop x input)
        "subagent-start" (on-subagent-start x input)
        "subagent-stop" (on-subagent-stop x input)
        nil))))

(defn handle!
  "Handle one hook event live, in the trace repo g/*repo*."
  [event input & [raw]]
  (handle-in! {:s (store/live g/*repo*) :d (disk/live g/*repo*) :probe? *probe?*} event input raw))

(defn -main [& [event]]
  (let [raw (slurp *in*)
        input (try (json/parse-string raw) (catch Exception _ {}))]
    (try
      (binding [*probe?* true] (handle! event input raw))
      (catch Throwable e
        (try (spit (gitdir "trace-errors.log")
                   (str (java.time.Instant/now) " " event " " (get input "session_id") " " (ex-message e) "\n")
                   :append true)
             (catch Throwable _))))
    (System/exit 0)))
