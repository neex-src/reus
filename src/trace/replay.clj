(ns trace.replay
  "Old sessions into a trace repo: the hook payloads are rebuilt from the transcripts
  and fed, in time order, to trace.record, which runs against a batch store (trace.store: one git fast-import) and a
  shadow of each project as it was then (trace.shadow), with git dates set to the event times.

    bb trace:replay <sel>... [--repo dir] [--map orig=dir]... [--force]
  sel as in jsonl:ls (several add up); every selected session, with its subagent runs, is one stream
  sorted by time, so prompt numbers follow reality across concurrent sessions. The repo
  (default var/replay; never the live one) is refused when it exists, unless --force
  replaces it.

  From a transcript:
    prompt          what the live hook was seen to get: a typed prompt (also one typed
                    into a running turn, a queue `remove`), a slash command as `/name args`,
                    an agent's <agent-message> hand-back, a <task-notification> unless it
                    only says the report came as such a message. Not: compaction
                    summaries, interrupt markers, other meta messages.
    tool            a tool_use with its tool_result, at the result's time; tool_response is
                    the entry's toolUseResult; is_error -> PostToolUseFailure with `error`
    stop            at the turn's stop-hook summary (transcripts without them: at an
                    end_turn message), with the turn's last text; a prompt that comes
                    while a turn runs does not end it; SessionEnd at the end of the file
    subagent-*      an agent file's first / last entry; its calls carry agent_id/agent_type
  Each payload has `replay` {file line}, kept in its note. An event also carries what the
  transcript adds to it (trace.turn), kept as the turn note, and file-history backups are
  evidence of a file's content when they were made.

  The trace repo holds its own objects: the trees a commit is based on are copied in from the
  project repos found in the sessions' cwds before fast-import is told of them; the CLAUDE.md files and memory files in
  .claude-trace/ are what the transcripts proved (instructions, memory and file attachments,
  edits to those files).

  Before record sees a tool event, the shadow is moved to the call's start and gets what
  the transcript proves, then to the call's end (a commit the call made): an
  Edit/MultiEdit/Write's resulting file (from originalFile and the input), a Bash call's
  bashEditDiff hunks, a full Read's content. Worktrees are the ones a transcript adds
  (`git worktree add`) plus the repo's current ones."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jsonl.read :as jr]
            [trace.git :as g]
            [trace.record :as rec]
            [trace.shadow :as shadow]
            [trace.store :as store]
            [trace.turn :as turn])
  (:import [java.time Instant]))

(def default-repo (str (fs/path (fs/parent g/default-repo) "replay")))

(defn- ms [ts] (try (.toEpochMilli (Instant/parse ts)) (catch Exception _ nil)))

(defn- lines
  "[line-no entry] of a transcript, 1-based, unparsable lines skipped."
  [f]
  (with-open [r (io/reader (str f))]
    (into [] (keep-indexed (fn [i l] (when-let [e (try (json/parse-string l) (catch Exception _ nil))] [(inc i) e])))
          (line-seq r))))

(defn- blocks [e] (let [c (get-in e ["message" "content"])] (if (sequential? c) c [])))
(defn- texts [e] (str/join "\n" (keep #(when (= "text" (get % "type")) (get % "text")) (blocks e))))
(defn- result-text [c] (if (string? c) c (str/join "\n" (keep #(get % "text") c))))
(defn- tag [s t] (some-> (re-find (re-pattern (str "(?s)<" t ">(.*?)</" t ">")) (str s)) second str/trim))

(defn- handed-back?
  "A notification that an agent finished whose report came as an <agent-message>: the
  message was the prompt, the notification is not (seen in the live traces)."
  [p]
  (boolean (and (re-find #"<summary>Agent .* finished</summary>" p)
                (str/includes? p "report was delivered to you as a message"))))

(defn- agent-message [t]
  (when-let [i (str/index-of t "<agent-message")]
    (let [s (subs t i) j (str/index-of s "</agent-message>")]
      (if j (subs s 0 (+ j (count "</agent-message>"))) s))))

(defn- prompt-of
  "The prompt a user/queue entry submitted, or nil. What makes a UserPromptSubmit was
  read off the live traces: typed prompts (also typed ahead into a running turn: a queue
  `remove`), an agent's hand-back <agent-message>, a task notification unless handed back,
  slash commands."
  [e]
  (let [c (get-in e ["message" "content"])
        t (if (string? c) c (str/join "\n" (keep #(get % "text") (blocks e))))]
    (cond
      (= "queue-operation" (get e "type"))
      (let [q (str (get e "content"))]
        (when (and (= "remove" (get e "operation")) (not (str/blank? q)) (not (handed-back? q)))
          (if (str/starts-with? q "<agent-message") (agent-message q) q)))
      (not= "user" (get e "type")) nil
      (get e "isMeta") (when (str/starts-with? t "Another Claude session sent a message:") (agent-message t))
      (str/starts-with? t "[Request interrupted by user") nil
      (str/starts-with? t "<command-name>") (str/trim (str (tag t "command-name") " " (tag t "command-args")))
      (str/starts-with? t "<task-notification>") (when-not (handed-back? t) t)
      :else (jr/prompt-text (json/parse-string (json/generate-string e) true)))))

(defn- event [kind ts file line payload & [extra]]
  (merge {:kind kind :ts ts :file (str file) :line line
          :payload (assoc payload "replay" {"file" (str file) "line" line})}
         extra))

(defn- distinct-by-key [evs] (vals (into (sorted-map) (map (juxt (juxt :path :backup) identity)) (reverse evs))))

(defn- tool-events
  "Tool events of a transcript's entries; agent: {agent_id agent_type} or nil."
  [f es sid agent]
  (let [uses (into {} (for [[_ e] es b (blocks e) :when (= "tool_use" (get b "type"))]
                        [(get b "id") (assoc b :start (ms (get e "timestamp")))]))
        ix (turn/index (mapv second es))]
    (for [[n e] es :when (= "user" (get e "type"))
          b (blocks e) :when (= "tool_result" (get b "type"))
          :let [u (get uses (get b "tool_use_id"))] :when u]
      (let [err? (true? (get b "is_error"))
            r (get e "toolUseResult")]
        (event :tool (ms (get e "timestamp")) f n
               (cond-> (merge {"session_id" sid "cwd" (get e "cwd")
                               "hook_event_name" (if err? "PostToolUseFailure" "PostToolUse")
                               "tool_name" (get u "name") "tool_input" (get u "input")
                               "tool_use_id" (get u "id")
                               "tool_response" (if (some? r) r (get b "content"))}
                              agent)
                 err? (assoc "error" (result-text (get b "content"))))
               {:branch (get e "gitBranch") :result r :start (:start u)
                :turn (turn/tool ix (get u "id") {})})))))

(def ^:dynamic *history-dir* "Where Claude Code keeps file backups." (str (fs/path (fs/home) ".claude" "file-history")))

(defn backup-events
  "A file's content as Claude Code backed it up before an edit (file-history-snapshot entries,
  backups under ~/.claude/file-history/<session>/): evidence of the file at backupTime."
  [f es sid]
  (distinct-by-key
   (for [[n e] es :when (= "file-history-snapshot" (get e "type"))
         [k {:strs [backupFileName backupTime realParentDir]}] (get-in e ["snapshot" "trackedFileBackups"])
         :when (and backupFileName realParentDir (ms backupTime))]
     (event :backup (ms backupTime) f n {}
            {:path (str realParentDir "/" (fs/file-name k)) :backup (str (fs/path *history-dir* sid backupFileName))}))))

(defn- attached
  "[path content] of the instruction, memory and file attachments recorded as a turn starts
  (between the prompt at position i of es and the model's first reply): what the transcript
  proves about the CLAUDE.md files and memory files in effect."
  [es i]
  (for [[_ e] (take-while #(not= "assistant" (get (second %) "type")) (subvec es (inc i) (min (count es) (+ i 400))))
        :when (= "attachment" (get e "type"))
        :let [a (get e "attachment")]
        [path content] (case (get a "type")
                         "instructions" (map (juxt #(get % "path") #(get % "content")) (get a "files"))
                         "nested_memory" [[(or (get a "path") (get-in a ["file" "path"])) (or (get a "content") (get-in a ["file" "content"]))]]
                         "file" [[(get a "filename") (get-in a ["content" "file" "content"])]]
                         nil)
        :when (and (string? path) (string? content))]
    [path content]))

(defn- session-events
  "Prompts, tool calls, stops and SessionEnd of a main transcript. A turn ends at its
  stop-hook summary (older transcripts without them: at an end_turn message); a prompt
  that comes while a turn runs (typed ahead, an agent's hand-back) does not end it."
  [f]
  (let [es (lines f)
        sid (or (some #(get (second %) "sessionId") es) (str (fs/strip-ext (fs/file-name f))))
        summaries? (some #(= "stop_hook_summary" (get (second %) "subtype")) es)
        cwd-at (fn [i] (or (some #(get (second %) "cwd") (rseq (subvec es 0 (inc i)))) (some #(get (second %) "cwd") es)))
        ix (turn/index (mapv second es))
        stop (fn [st ts n i]
               (-> st
                   (update :out conj (event :stop ts f n {"session_id" sid "hook_event_name" "Stop"
                                                          "last_assistant_message" (or (get-in st [:open :text]) "")}
                                            {:turn (turn/stop ix (inc i))}))
                   (assoc :open nil)))
        step (fn [{:keys [open seen] :as st} [i [n e]]]
               (let [p (prompt-of e)
                     ;; an agent message can come both from the queue and as an entry
                     task (when (and p (str/starts-with? p "<agent-message")) p)
                     ts (ms (get e "timestamp"))]
                 (cond
                   (and p (not (and task (seen task))))
                   (-> st
                       (update :out conj (event :prompt ts f n
                                                {"session_id" sid "cwd" (cwd-at i)
                                                 "hook_event_name" "UserPromptSubmit" "prompt" p}
                                                {:branch (get e "gitBranch") :turn (turn/prompt ix i) :trace-attach (attached es i)}))
                       (assoc :open {})
                       (cond-> task (update :seen conj task)))

                   (and open (= "assistant" (get e "type")))
                   (let [t (texts e)
                         st (cond-> st (not (str/blank? t)) (assoc-in [:open :text] t))]
                     (if (and (not summaries?) (= "end_turn" (get-in e ["message" "stop_reason"])))
                       (stop st ts n i)
                       st))

                   (and open (= "system" (get e "type")) (= "stop_hook_summary" (get e "subtype")))
                   (stop st ts n i)

                   :else st)))
        {:keys [out]} (reduce step {:out [] :seen #{}} (map-indexed vector es))
        last-ts (some #(ms (get (second %) "timestamp")) (rseq es))
        last-text (some #(let [t (when (= "assistant" (get (second %) "type")) (texts (second %)))]
                           (when-not (str/blank? t) t))
                        (rseq es))]
    (concat out
            (when (seq es)
              [(event :stop last-ts f (first (peek es))
                      {"session_id" sid "hook_event_name" "SessionEnd" "reason" "replay"
                       "last_assistant_message" (or last-text "")}
                      {:turn (turn/stop ix)})])
            (backup-events f es sid)
            (tool-events f es sid nil))))

(defn- agent-events [session-file]
  (let [sid (str (fs/strip-ext (fs/file-name session-file)))
        dir (fs/path (fs/parent session-file) sid "subagents")]
    (when (fs/directory? dir)
      (mapcat
       (fn [f]
         (let [id (second (re-find #"agent-(.+)\.jsonl$" (str f)))
               meta (let [m (fs/path dir (str "agent-" id ".meta.json"))]
                      (when (fs/exists? m) (json/parse-string (slurp (str m)))))
               es (lines f)
               a {"agent_id" id "agent_type" (or (get meta "agentType") "")}
               ts (keep #(ms (get (second %) "timestamp")) es)
               last-text (some #(let [t (when (= "assistant" (get (second %) "type")) (texts (second %)))]
                                  (when-not (str/blank? t) t))
                               (rseq es))]
           (when (seq es)
             (concat
              [(event :subagent-start (first ts) f (first (first es))
                      (merge {"session_id" sid "cwd" (some #(get (second %) "cwd") es)
                              "hook_event_name" "SubagentStart"} a)
                      {:branch (some #(get (second %) "gitBranch") es)})]
              (tool-events f es sid a)
              [(event :subagent-stop (last ts) f (first (peek es))
                      (merge {"session_id" sid "hook_event_name" "SubagentStop"
                              "last_assistant_message" (or last-text "")} a))]))))
       (sort (map str (fs/glob dir "agent-*.jsonl")))))))

(def ^:private order {:backup -1 :prompt 0 :subagent-start 1 :tool 2 :subagent-stop 3 :stop 4})

(defn events
  "Every event of the given main transcripts and their agents, in time order."
  [files]
  (->> files
       (mapcat #(concat (session-events %) (agent-events %)))
       (filter :ts)
       (sort-by (juxt :ts #(order (:kind %)) :file :line))))

;; --- worktrees named by the transcripts ---------------------------------------------

(defn worktree-adds
  "Orig dirs that `git worktree add` calls created: [[dir branch]], resolved against the
  call's cwd (and a leading `cd x &&`, or `git -C x`); branch is the -b/-B one, else a
  second argument naming a branch, else nil."
  [evs]
  (for [{:keys [kind payload]} evs :when (= kind :tool)
        :let [cmd (str (get-in payload ["tool_input" "command"]))]
        [_ pre c-dir args] (re-seq #"(?:cd\s+(\S+)\s*&&\s*)?git\s+(?:-C\s+(\S+)\s+)?worktree\s+add\s+([^;&|\n]+)" cmd)
        :let [toks (remove #(re-find #"^\d?>" %) (str/split (str/trim args) #"\s+"))
              {:keys [b pos detach]}
              (loop [[t & more] toks o {:pos []}]
                (cond (nil? t) o
                      (#{"-b" "-B"} t) (recur (rest more) (assoc o :b (first more)))
                      (= "--reason" t) (recur (rest more) o)
                      (= "--detach" t) (recur more (assoc o :detach true))
                      (str/starts-with? t "-") (recur more o)
                      :else (recur more (update o :pos conj t))))
              path (first pos)
              branch (or b (when-not detach (second pos)))
              base (reduce (fn [b d] (if d (str (fs/path b d)) b))
                           (get payload "cwd") [pre c-dir])]
        :when (and path base)]
    [(str (fs/normalize (fs/path base (str/replace path #"^['\"]|['\"]$" "")))) branch]))

;; --- what the transcript proves about the files --------------------------------------

(defn- split-keep [s] (let [ls (str/split (str s) #"\n" -1)] (if (= "" (peek ls)) [(pop ls) true] [ls false])))

(defn patch
  "Apply structured-patch hunks to old (nil = no file). [new-content-or-nil mismatches]."
  [old hunks]
  (let [[ls nl?] (if old (split-keep old) [[] true])
        ls (vec ls)
        {:keys [out i bad nl?]}
        (reduce (fn [{:keys [i] :as st} {:strs [oldStart lines]}]
                  (let [to (max i (dec (or oldStart 1)))
                        st (-> st (update :out into (subvec ls (min i (count ls)) (min to (count ls)))) (assoc :i to))]
                    (reduce (fn [{:keys [i] :as st} l]
                              (let [c (first l) body (subs l (min 1 (count l)))]
                                (case c
                                  \+ (update st :out conj body)
                                  \\ (assoc st :nl? false)
                                  (-> st (update :i inc)
                                      (cond-> (= c \space) (update :out conj body))
                                      (cond-> (not= body (get ls i)) (update :bad inc))))))
                            st lines)))
                {:out [] :i 0 :bad 0 :nl? nl?} hunks)
        out (into out (subvec ls (min i (count ls))))]
    [(when (or (seq out) (some #(some (fn [l] (str/starts-with? l "+")) (get % "lines")) hunks))
       (str (str/join "\n" out) (when (and nl? (seq out)) "\n")))
     bad]))

(defn- edited
  "The file after an edit call, from the transcript, or nil."
  [sh {:strs [tool_name tool_input]} r]
  (let [base #(or (when (map? r) (get r "originalFile")) (shadow/content sh %))
        repl (fn [s {:strs [old_string new_string replace_all]}]
               (cond (or (nil? s) (nil? old_string) (nil? new_string)) s
                     replace_all (str/replace s old_string new_string)
                     :else (str/replace-first s old_string new_string)))
        p (get tool_input "file_path")]
    (when (and p (map? r))
      (case (str/replace (str tool_name) #"^mcp__.*__" "")
        "Write" [p (or (get r "content") (get tool_input "content"))]
        "Edit" (when-let [b (base p)] [p (repl b tool_input)])
        "MultiEdit" (when-let [b (base p)] [p (reduce repl b (get tool_input "edits"))])
        nil))))

(defn- stat! [st k] (swap! st update k (fnil inc 0)))

(defn- evidence!
  "Write into the shadow what a tool event proves. Counted in st."
  [sh st {:keys [payload result ts start] :as ev}]
  (when-not (= "PostToolUseFailure" (get payload "hook_event_name"))
    (when-let [[p c] (edited sh payload result)]
      (when c (shadow/trace-file! sh (get payload "session_id") p c))
      (when (and c (shadow/write! sh p c)) (stat! st :edit)))
    (doseq [{:strs [filePath hunks]} (when (map? result) (get-in result ["bashEditDiff" "files"]))]
      (shadow/sync! sh filePath (or start ts) nil)
      (when (shadow/writable? sh filePath)
        (let [[c bad] (patch (shadow/content sh filePath) hunks)]
          (if c (shadow/write! sh filePath c) (shadow/delete! sh filePath))
          (stat! st :bash-diff)
          (when (pos? bad)
            (stat! st :bash-diff-mismatch)
            (swap! st update :mismatched (fnil conj []) [(:file ev) (:line ev) filePath bad])))))
    (when-let [{:strs [filePath content numLines totalLines startLine]}
               (when (and (map? result) (= "text" (get result "type"))) (get result "file"))]
      (when (and filePath (string? content) (= 1 startLine) (= numLines totalLines)
                 (shadow/writable? sh filePath) (not= content (shadow/content sh filePath)))
        (shadow/write! sh filePath content)
        (stat! st :read-fix)))))

;; --- run ------------------------------------------------------------------------------

(defn- file-of [{:strs [tool_input]}] (some #(get tool_input %) ["file_path" "path" "notebook_path"]))

(defn- backup!
  "Put a backed-up file into the shadow when it is not what the shadow has."
  [sh st {:keys [ts path backup]}]
  (when (fs/regular-file? backup)
    (let [bs (fs/read-all-bytes backup) c (String. bs "UTF-8")]
      (when (java.util.Arrays/equals ^bytes (.getBytes c "UTF-8") ^bytes bs) ; text only
        (shadow/sync! sh path ts nil)
        (stat! st :backup)
        (when (not= c (shadow/content sh path))
          (shadow/write! sh path c)
          (stat! st :backup-applied))))))

(def ^:private record-kinds #{:prompt :tool :stop :subagent-start :subagent-stop})

(defn run-events!
  "Feed events evs (in time order) to trace.record, against a batch store for repo (a new one) and a
  shadow of the projects (maps {orig-root repo-dir}). A tool event's evidence goes into the shadow by
  (evidence sh st ev) on the disk as it was when the call started; an event of a kind trace.record does
  not know (a backup, a check) goes to (side sh st ev). st is the stats atom both may count in. Returns
  the stats."
  [evs repo maps {:keys [evidence side]}]
  (let [sh (shadow/create maps)
        _ (doseq [[wt branch] (distinct (worktree-adds evs))] (shadow/worktree! sh wt branch))
        _ (doseq [cwd (distinct (keep #(get-in % [:payload "cwd"]) evs))] (shadow/open! sh cwd))
        store (store/batch repo)
        x {:s store :d (shadow/disk sh repo)}
        st (atom {})]
    (try
      (doseq [{:keys [kind ts start payload branch] :as ev} evs]
        (if-not (record-kinds kind)
          (side sh st ev)
        (let [cwd (get payload "cwd")
              f (when (= kind :tool) (let [f (file-of payload)] (when (and f (fs/absolute? f)) f)))
              sync! (fn [t] (when f (shadow/sync! sh f t branch))
                      (when cwd (shadow/sync! sh cwd t branch)))]
          ;; a tool call's evidence applies to the disk as it was when the call started; a
          ;; commit the call made shows when the disk is then moved to the call's end
          (when (= kind :tool)
            (sync! (or start ts))
            (evidence sh st ev))
          (when (and cwd (not (sync! ts))) (stat! st :no-repo))
          (store/at! store ts)
          (shadow/at! sh ts)
          (doseq [[path content] (:trace-attach ev)] (shadow/trace-file! sh (get payload "session_id") path content))
          (rec/handle-in! (assoc x :turn (:turn ev)) (name kind) payload)
          (stat! st kind))))
      (store/close! store)
      (catch Throwable e (.destroy ^Process (:proc store)) (throw e))
      (finally (shadow/close! sh)))
    (merge @st {:head (shadow/stats sh)})))

(defn replay!
  "Replay main transcripts files into repo (a new one); maps {orig-root repo-dir}. Returns stats."
  [files repo maps]
  (run-events! (events files) repo maps {:evidence evidence! :side backup!}))

(defn -main [& argv]
  (let [{:keys [sel repo maps force]}
        (loop [[a b & more :as as] argv o {:maps {}}]
          (cond (empty? as) o
                (= a "--repo") (recur more (assoc o :repo b))
                (= a "--map") (recur more (update o :maps conj (str/split b #"=" 2)))
                (= a "--force") (recur (rest as) (assoc o :force true))
                :else (recur (rest as) (update o :sel (fnil conj []) a))))
        repo (str (fs/absolutize (or repo default-repo)))
        files (distinct (mapcat jr/files sel))]
    (cond
      (empty? files) (do (println "usage: bb trace:replay <sel>... [--repo dir] [--map orig=dir]... [--force]"
                                  (if sel (str "\nno transcripts for " (str/join " " sel)) "")) (System/exit 1))
      (some #{repo} (map #(str (fs/absolutize %)) [g/default-repo g/*repo*]))
      (do (println "refusing to replay into the live trace repo" repo) (System/exit 1))
      (and (fs/exists? repo) (not force))
      (do (println repo "exists; --force replaces it") (System/exit 1))
      :else
      (do (when (fs/exists? repo) (fs/delete-tree repo))
          (println "replaying" (count files) "sessions into" repo)
          (doseq [f files] (println " " f))
          (let [s (replay! files repo maps)]
            (println (pr-str (update s :mismatched #(take 20 %)))))))))
