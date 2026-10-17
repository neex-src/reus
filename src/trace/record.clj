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

  Nothing is ever checked out: commits are built with a private index and
  commit-tree and moved with update-ref, under one mkdir lock, so concurrent
  sessions cannot disturb each other. The open prompt of a session is
  .git/trace-sessions/<session id>. Errors go to .git/trace-errors.log; the hook
  always exits 0, so tracing never breaks a session.

  trace.replay feeds old transcripts through the same handlers: *local* / *orig* then
  lead a session's paths into a shadow of the project as it was (trace.shadow)."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [trace.agents :as agents]
            [trace.git :as g :refer [git]]
            [trace.probe :as probe]))

(def delim "--- error ---")

(def ^:dynamic *local*
  "Path as a session named it -> where it is on disk now. Identity, except in a replay,
  where a session's paths lead into a shadow checkout of how the project was then."
  identity)

(def ^:dynamic *orig*
  "The inverse of *local*: a disk path -> the path as the session knew it."
  identity)
(def edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})
(def read-tools #{"Read" "NotebookRead" "read_file"})

(defn- gitdir [& more] (str (apply fs/path g/*repo* ".git" more)))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "...") s)))
(defn- first-line [s n] (clip (first (str/split-lines (str s))) n))
(defn- short-tool [t] (str/replace (str t) #"^mcp__.*__" ""))

;; --- repo plumbing ----------------------------------------------------------

(defn ensure-repo! []
  (when-not (fs/exists? (gitdir))
    (fs/create-dirs g/*repo*)
    (git "init" "-q" "-b" "_" {:dir g/*repo*})
    (fs/create-dirs (gitdir "trace-sessions"))))

(defn- with-lock [f]
  (let [lock (gitdir "trace.lock")]
    (loop [n 0]
      (if (try (fs/create-dir lock) true (catch Exception _ false))
        (try (f) (finally (fs/delete-tree lock)))
        (if (< n 300)
          (do (Thread/sleep 50) (recur (inc n)))
          ;; a lock this old is a crashed hook's; take it over
          (do (fs/delete-tree lock) (recur 0)))))))

(defn- tip [ref]
  (let [r (g/git* {} "rev-parse" "--verify" "-q" (if (str/starts-with? ref "refs/") ref (str "refs/heads/" ref)))]
    (when (zero? (:exit r)) (str/trim (:out r)))))

(defn- blob! [content] (git "hash-object" "-w" "--stdin" {:in (str content)}))

(defn- commit!
  "A commit on top of parent whose tree is parent's plus files {path content};
  merge-parent makes it a merge. Returns its sha."
  [parent files subject body & [merge-parent]]
  (let [env {:extra-env {"GIT_INDEX_FILE" (gitdir "trace-index")}}]
    (if parent (git "read-tree" parent env) (git "read-tree" "--empty" env))
    (doseq [[path content] files]
      (git "update-index" "--add" "--cacheinfo" (str "100644," (blob! content) "," path) env))
    (apply git "commit-tree" (git "write-tree" env)
           (concat (mapcat #(vector "-p" %) (remove nil? [parent merge-parent]))
                   ["-m" subject] (when-not (str/blank? body) ["-m" body])))))

(def ^:dynamic *raw* "The hook stdin of the event being handled, as received." nil)
(def ^:dynamic *probe?* "Measure the transcript at hook time (the live hook; not a replay)." false)
(def ^:dynamic *probed* "The probe of the event being handled." nil)

(def notes-ref "refs/notes/event")
(def probe-ref "refs/notes/probe")

(defn- add-note!
  "Note content on commit sha in ref (notes tree built like commit!)."
  [ref sha content]
  (let [env {:extra-env {"GIT_INDEX_FILE" (gitdir "trace-index")}}
        parent (tip ref)]
    (if parent (git "read-tree" parent env) (git "read-tree" "--empty" env))
    (git "update-index" "--add" "--cacheinfo" (str "100644," (blob! content) "," sha) env)
    (git "update-ref" ref
         (apply git "commit-tree" (git "write-tree" env)
                (concat (when parent ["-p" parent]) ["-m" (last (str/split ref #"/"))])))))

(defn- note!
  "Keep the event's raw payload (and its probe) as notes on commit sha."
  [sha]
  (when *raw*
    (add-note! notes-ref sha *raw*)
    (when *probed* (add-note! probe-ref sha (pr-str *probed*)))))

(defn- advance! [branch sha] (git "update-ref" (str "refs/heads/" branch) sha))
(defn- has? [rev path] (zero? (:exit (g/git* {} "cat-file" "-e" (str rev ":" path)))))

;; --- sessions -----------------------------------------------------------------

(defn- state-file [sid] (gitdir "trace-sessions" (str/replace (str sid) #"[^A-Za-z0-9._-]" "_")))
(defn- state [sid] (let [f (state-file sid)] (when (fs/exists? f) (edn/read-string (slurp f)))))
(defn- state! [sid m]
  (let [f (state-file sid)]
    (if m (do (fs/create-dirs (fs/parent f)) (spit f (pr-str m))) (fs/delete-if-exists f))))

(defn- last-file [sid] (gitdir "trace-last" (str/replace (str sid) #"[^A-Za-z0-9._-]" "_")))
(defn- last-prompt
  "The session's most recent prompt {:project :branch :nn :root}, open or not."
  [sid]
  (let [f (last-file sid)] (when (fs/exists? f) (edn/read-string (slurp f)))))

(defn- agent-key [agent-id] (str "agent-" agent-id))

(defn- pending-file [agent-id] (gitdir "trace-pending" (str/replace (str agent-id) #"[^A-Za-z0-9._-]" "_")))
(defn- pending!
  "A finished agent whose parent was idle: its tip waits for the prompt that reacts to its notification."
  [agent-id m] (let [f (pending-file agent-id)] (fs/create-dirs (fs/parent f)) (spit f (pr-str m))))
(defn- take-pending! [agent-id]
  (let [f (pending-file agent-id)]
    (when (fs/exists? f) (let [m (edn/read-string (slurp f))] (fs/delete f) (:tip m)))))

(defn project-of
  "{:project name :root dir} for a session cwd, or nil when it is not traced."
  [cwd]
  (when (and cwd (fs/directory? (*local* cwd)))
    (let [disk (str (fs/canonicalize (*local* cwd)))
          cwd (*orig* disk)
          r (p/shell {:out :string :err :string :continue true :dir disk}
                     "git" "rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel")
          projects (str (fs/path (fs/home) "projects"))
          clean #(str/replace % #"[^A-Za-z0-9._-]" "_")]
      (cond
        (str/starts-with? (str disk "/") (str (fs/canonicalize (fs/create-dirs g/*repo*)) "/")) nil
        (zero? (:exit r))
        (let [[common top] (map *orig* (str/split-lines (str/trim (:out r))))
              home (if (= ".git" (str (fs/file-name common))) (fs/parent common) top)]
          {:project (clean (str (fs/file-name home))) :root top})
        :else
        (when-let [[_ n] (re-find (re-pattern (str "^\\Q" projects "\\E/([^/]+)")) cwd)]
          {:project (clean n) :root (str (fs/path projects n))})))))

(defn- next-n [project]
  (let [names (str/split-lines (git "for-each-ref" "--format=%(refname:short)" (str "refs/heads/" project "-p*")))]
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
  [root {:strs [tool_input cwd]}]
  (let [resolve (fn [tok]
                  (let [tok (str/replace (str tok) #"^['\"]|['\"]$" "")
                        f (cond (str/blank? tok) nil
                                (fs/absolute? tok) (*local* tok)
                                :else (some #(let [c (str (fs/path (*local* %) tok))] (when (fs/exists? c) c)) [(or cwd root) root]))]
                    (when (and f (fs/regular-file? f)) (*orig* (str (fs/canonicalize f))))))]
    (if-let [fp (some #(get tool_input %) ["file_path" "path" "notebook_path"])]
      (resolve fp)
      (when-let [cmd (get tool_input "command")]
        (let [fs* (distinct (filter #(str/starts-with? % (str root "/"))
                                    (keep resolve (str/split cmd #"[\s;|&<>()]+"))))]
          (when (= 1 (count fs*)) (first fs*)))))))

(defn- before-edit
  "The file as it was before this edit, as well as can be told after it."
  [root rel now {:strs [tool_name tool_input]}]
  (let [undo (fn [s {:strs [old_string new_string replace_all]}]
               (cond (or (nil? new_string) (nil? old_string)) s
                     replace_all (str/replace s new_string old_string)
                     :else (str/replace-first s new_string old_string)))]
    (case (short-tool tool_name)
      "Edit" (undo now tool_input)
      "MultiEdit" (reduce undo now (reverse (get tool_input "edits")))
      (let [r (p/shell {:out :string :err :string :continue true :dir (*local* root)} "git" "show" (str "HEAD:" rel))]
        (if (zero? (:exit r)) (:out r) "")))))

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

(defn- close!
  "Reply to the open prompt of key (a session id, or an agent's key) and merge it into
  its project (--no-ff: the merge's tree is the prompt's). An agent's branch is not merged."
  [key reply & [note?]]
  (when-let [{:keys [project branch nn subject agent merge]} (state key)]
    (when-let [t (tip branch)]
      (let [r (commit! t {"doto.md" reply} (str "response " nn) "")]
        (advance! branch r)
        (when note? (note! r))
        (when-not agent
          ;; finished agents whose notification this prompt reacted to are merged with it
          (advance! project (apply git "commit-tree" (str r "^{tree}") "-p" (tip project) "-p" r
                                   (concat (mapcat #(vector "-p" %) merge) ["-m" subject]))))))
    (state! key nil)))

(defn on-prompt [{:strs [session_id prompt cwd]}]
  (close! session_id "")
  (when-let [{:keys [project root]} (project-of cwd)]
    (when-not (tip project)
      (advance! project (commit! nil {} (str "trace of " project) (str "root " root))))
    (let [nn (format "%02d" (next-n project))
          branch (str project "-p" nn)
          subject (str "prompt " nn ": " (clip prompt 100))
          c (commit! (tip project) {"doto.md" prompt} subject
                     (str "session " session_id "\ncwd " cwd "\nroot " root))]
      (advance! branch c)
      (note! c)
      (when-let [f (last-file session_id)]
        (fs/create-dirs (fs/parent f))
        (spit f (pr-str {:project project :branch branch :nn nn :root root})))
      (state! session_id {:project project :branch branch :nn nn :root root :k 0 :subject subject
                          :merge (vec (keep take-pending! (map second (re-seq #"<task-id>([A-Za-z0-9]+)</task-id>" (str prompt)))))}))))

(defn on-tool [{:strs [session_id agent_id tool_name tool_input hook_event_name] :as in}]
  (let [key (if agent_id (agent-key agent_id) session_id)]
   (when-let [{:keys [branch nn root k] :as st} (state key)]
    (when-let [t (tip branch)]
      (let [k (inc k)
            tool (short-tool tool_name)
            cmd (command-of in)
            text (text-of (get in "tool_response"))
            text (if (= hook_event_name "PostToolUseFailure") (or (get in "error") text) text)
            bad? (failed? in text)
            file (the-file root in)
            rel (some->> file (rel-of root))
            agent (when-let [a (get in "agent_type")] (str "\n\nagent " a))
            c (cond
                (and file (edit-tools tool) (not bad?))
                (let [now (slurp (*local* file))
                      path (str "files/" rel)
                      t (if (has? t path)
                          t
                          (commit! t {path (before-edit root rel now in)} (str "baseline " rel)
                                   "the file before its first traced edit"))]
                  (commit! t {path now} (str tool ": " (first-line rel 90)) (str cmd agent)))

                (and file (or (read-tools tool) (get tool_input "command")))
                (commit! t {(str "reads/" rel) (if bad? (str delim "\n" text) text)}
                         (git "hash-object" "-w" (*local* file)) (str cmd agent))

                :else
                (commit! t {(format "reads/_cmd/%s.%d" nn k) (if bad? (str delim "\n" text) text)}
                         (format "%s.%d" nn k) (str cmd agent)))]
        (advance! branch c)
        (note! c)
        (state! key (assoc st :k k)))))))

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

(defn on-subagent-start [{:strs [session_id agent_id agent_type cwd]}]
  (when-let [{:keys [project branch nn root]} (or (state session_id) (last-prompt session_id))]
    (when-let [t (and agent_id (tip branch))]
      (let [anid (str nn "-a" agent_id)
            ab (str project "-p" anid)
            brief (agent-brief session_id agent_id)
            subject (str "prompt " anid ": " (clip (str agent_type ": " (first-line brief 80)) 100))
            c (commit! t {"doto.md" brief} subject
                       (str "session " session_id "\ncwd " cwd "\nroot " root "\nagent " agent_type
                            "\nagent_id " agent_id "\nparent " branch))]
        (advance! ab c)
        (note! c)
        (state! (agent-key agent_id) {:project project :branch ab :nn anid :root root :k 0 :subject subject
                                      :agent true :session session_id})))))

(defn on-subagent-stop
  "Close the agent's branch. Its tip is merged where the parent reacts to the agent's
  notification: into the parent's prompt when one is open (the notification is delivered
  into that turn), else into the prompt that is opened by the notification."
  [{:strs [session_id agent_id last_assistant_message]}]
  (when agent_id
    (let [b (:branch (state (agent-key agent_id)))]
      (close! (agent-key agent_id) (or last_assistant_message "") true)
      (when-let [t (and b (tip b))]
        (if-let [st (state session_id)]
          (state! session_id (update st :merge (fnil conj []) t))
          (pending! agent_id {:tip t :session session_id}))))))

(defn- agent-states-of [session-id]
  (let [d (gitdir "trace-sessions")]
    (when (fs/exists? d)
      (keep #(let [st (try (edn/read-string (slurp (str %))) (catch Exception _ nil))]
               (when (and (:agent st) (= session-id (:session st))) (str/replace (str (fs/file-name %)) #"^agent-" "")))
            (fs/list-dir d)))))

(defn on-stop [{:strs [session_id hook_event_name] :as in}]
  (close! session_id (reply-of in) true)
  (when (= "SessionEnd" hook_event_name)
    (doseq [a (agent-states-of session_id)] (close! (agent-key a) "" false))))

(defn handle!
  "Handle one hook event; raw is its stdin as received (default: the parsed input re-encoded)."
  [event input & [raw]]
  (ensure-repo!)
  (binding [*raw* (or raw (json/generate-string input))
            *probed* (when *probe?* (probe/measure event input))]
    (with-lock
      #(case event
         "prompt" (on-prompt input)
         "tool" (on-tool input)
         "stop" (on-stop input)
         "subagent-start" (on-subagent-start input)
         "subagent-stop" (on-subagent-stop input)
         nil))))

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
