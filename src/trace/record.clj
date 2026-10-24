(ns trace.record
  "Global Claude Code hook: every session on this machine becomes git history in one
  trace repo (trace.git/*repo*), nothing squashed.

  Hook command:  bb --config <reus>/bb.edn -m trace.record <event>
    prompt          UserPromptSubmit
    tool            PostToolUse / PostToolUseFailure
    stop            Stop, SessionEnd
    subagent-start  SubagentStart
    subagent-stop   SubagentStop

  Refs. The project is the session's repo (the main checkout's dir name, also for a
  worktree of it); outside git, the ~/projects/<name> ($TRACE_PROJECTS/<name>) it sits in; elsewhere nothing is traced.
    <project>/main                 the real main, commit for commit
    <project>/s/<session8>         one Claude session
    <project>/a/<session8>-<agent> one subagent run
  (a ref cannot be `x` and `x/y` at once, hence the prefixes).

  A session branch: `prompt NN: ...`, one commit per tool call (`NN.k Tool: ...`), `response
  NN`, then the next prompt on the same branch; NN counts the session's prompts. Every
  commit's tree is the whole project tree of the session's worktree at that moment (tracked
  and untracked files that are not ignored, as `git add -A` has them: so a commit's diff is
  what that call changed, and a read is an empty diff), plus .claude-trace/ with prompt.md,
  reply.md, memory/ (the project's memory files) and claude-md/ (the CLAUDE.md chain in
  effect), snapshotted at each prompt. The trace repo holds every object it refers to (no
  alternates): a snapshot writes the blobs it hashes, a mirror commit copies the real commit's
  tree from the project repo, so the trace survives a project or worktree that moves or goes.

  Commit messages end in trailers: Tool, File, Exit, Error, Duration-Ms, Model, Tokens-In,
  Tokens-Out, Cache-Read, Cache-Write, Prompt-Id, Agent, Cwd, Real-Head (the worktree's HEAD
  at that moment), Agent-Report (the tip of an agent whose report this turn got, which is
  no parent link). Model and tokens come from the transcript; a payload without one may bring
  them as `model` and `usage` (moot feeds a deepseek exchange as UserPromptSubmit / Stop with
  last_assistant_message). Notes: refs/notes/event is the hook stdin verbatim, probe says whether the
  transcript already held the event (trace.probe), turn what the transcript adds (trace.turn),
  output the call's output text (a tool output Claude Code persisted to a file is recorded in
  full) or the output file of a background task a prompt's notification names.

  Trace main mirrors the real main's first-parent history: a commit per real commit, same
  tree, the real message and author plus `Real-Commit: <sha>`. A real merge gets as further
  parents the tips of the session and agent branches that did work in the commits it merged
  (those whose Real-Head is among them), so `git log -p <project>/main -- <file>` shows
  every tool call that led to the file. The mirror catches up whenever a hook sees that the
  real main moved.

  A subagent run is recorded like a session of its own: SubagentStart opens its branch, cut
  from the parent's tip, with `prompt NN-a<id>: ...` holding the agent's brief; every tool
  event that carries an agent_id goes to that branch; SubagentStop adds `response NN-a<id>`.
  SessionEnd closes agents still open.

  Commits are built with commit-tree (the tree from a private index per worktree and
  `git add -A`) and moved with update-ref, under one mkdir lock, so concurrent sessions
  cannot disturb each other. Session state is in .git/trace-sessions/<session id>. Errors go
  to .git/trace-errors.log; the hook always exits 0, so tracing never breaks a session.

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

(def edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})
(def read-tools #{"Read" "NotebookRead" "read_file"})

(def ^:dynamic *probe?* "Measure the transcript at hook time (the live hook; not a replay)." false)

(def notes-ref "refs/notes/event")
(def probe-ref "refs/notes/probe")
(def turn-ref "refs/notes/turn")
(def output-ref "refs/notes/output")

;; x is the context of one event: {:s store :d disk :raw payload as received :probed probe
;; :turn what the transcript adds :tkey the key of the session or agent}

(defn- gitdir [& more] (str (apply fs/path g/*repo* ".git" more)))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "...") s)))
(defn- first-line [s n] (clip (first (str/split-lines (str s))) n))
(defn- short-tool [t] (str/replace (str t) #"^mcp__.*__" ""))

(defn ensure-repo! [] (store/ensure! (store/live g/*repo*)))

(defn project-of
  "{:project name :root dir} for a session cwd, or nil when it is not traced."
  ([cwd] (project-of (disk/live g/*repo*) cwd))
  ([d cwd] (disk/project-of d cwd)))

;; --- the store, by branch -----------------------------------------------------------

(defn- head-ref [branch] (str "refs/heads/" branch))
(defn- tip [x branch] (store/tip (:s x) (head-ref branch)))
(defn- advance! [x branch id] (store/advance! (:s x) (head-ref branch) id))

(defn- note!
  "Keep the event's raw payload (and its probe, and what the transcript adds) as notes on commit sha."
  [x sha]
  (when (:raw x)
    (store/note! (:s x) notes-ref sha (:raw x))
    (when (:probed x) (store/note! (:s x) probe-ref sha (pr-str (:probed x))))
    (when-let [t (:turn x)]
      (store/note! (:s x) turn-ref sha (pr-str (dissoc t :pending-id)))
      (when-let [id (:pending-id t)] ; the tool_use was not in the transcript yet: looked for at the next hook
        (store/kv! (:s x) :turn-pending (:tkey x) {:id id :commit sha})))))

(defn- trailers
  "The trailer block of a commit message from [[key value]] (nil values left out)."
  [pairs]
  (str/join "\n" (keep (fn [[k v]] (when (and (some? v) (not (str/blank? (str v))))
                                     (str k ": " (str/replace (str v) #"\s*\n\s*" " "))))
                        pairs)))

(defn- with-trailers [body pairs]
  (let [t (trailers pairs)]
    (cond (str/blank? t) (str body)
          (str/blank? body) t
          :else (str body "\n\n" t))))

;; --- sessions -----------------------------------------------------------------

(defn- state [x key] (store/kv (:s x) :sessions key))
(defn- state! [x key m] (store/kv! (:s x) :sessions key m))

(defn- agent-key [agent-id] (str "agent-" agent-id))
(defn- s8 [session-id] (subs (str session-id) 0 (min 8 (count (str session-id)))))
(defn- main-branch [project] (str project "/main"))
(defn- session-branch [project session-id] (str project "/s/" (s8 session-id)))
(defn- agent-branch [project session-id agent-id] (str project "/a/" (s8 session-id) "-" agent-id))

(defn- pending!
  "A finished agent whose parent was idle: its tip waits for the prompt that reacts to its notification."
  [x agent-id m] (store/kv! (:s x) :pending agent-id m))
(defn- take-pending! [x agent-id]
  (when-let [m (store/kv (:s x) :pending agent-id)]
    (store/kv! (:s x) :pending agent-id nil)
    (:tip m)))

;; --- the mirror of the real main ------------------------------------------------------

(defn- note-head!
  "Remember that branch did work with the real commit sha checked out."
  [x sha branch]
  (when sha
    (let [cur (store/kv (:s x) :realheads sha)]
      (when-not (some #{branch} cur) (store/kv! (:s x) :realheads sha (conj (vec cur) branch))))))

(defn- add-trailer
  "message with a trailer line: in its trailer block when it ends in one, else after a blank line."
  [message k v]
  (let [m (str/trimr message)
        last-par (last (str/split m #"\n\n"))
        block? (and (str/includes? m "\n\n") (every? #(re-find #"^[A-Za-z][A-Za-z-]*: " %) (str/split-lines last-par)))]
    (str m (if block? "\n" "\n\n") k ": " v)))

(defn- mirror-commit!
  "Trace main gets a commit for real commit sha: its tree, message and idents, the tips of the
  branches that did work in what a merge brings in as further parents."
  [x p ref sha]
  (let [{:keys [tree parents author committer message]} (disk/commit-info (:d x) p sha)
        merged (when (> (count parents) 1)
                 (distinct (mapcat (fn [q] (mapcat #(store/kv (:s x) :realheads %) (disk/range-set (:d x) p (first parents) q)))
                                   (rest parents))))
        extra (keep #(tip x %) merged)
        parent (store/tip (:s x) ref)
        msg (add-trailer message "Real-Commit" sha)
        [subject body] (let [i (str/index-of msg "\n")] (if i [(subs msg 0 i) (str/triml (subs msg i))] [msg ""]))]
    (store/advance! (:s x) ref
                    (store/commit! (:s x) {:parents (vec (concat (when parent [parent]) extra)) :tree {:base tree :from (:common p)}
                                           :subject subject :body body :author author :committer committer}))))

(defn- mirror!
  "Bring trace main up to the real main: one commit per real first-parent commit since the last
  one mirrored (the first time, just the tip)."
  [x {:keys [project common] :as p}]
  (when common
    (let [tip* (disk/main-tip (:d x) p)
          last* (store/kv (:s x) :mirror project)]
      (when (and tip* (not= tip* last*))
        (doseq [sha (disk/chain (:d x) p last* tip*)]
          (mirror-commit! x p (head-ref (main-branch project)) sha))
        (store/kv! (:s x) :mirror project tip*)))))

(defn- ensure-main!
  "A project without a real main (or outside git) gets an empty root on trace main."
  [x {:keys [project root]}]
  (when-not (tip x (main-branch project))
    (advance! x (main-branch project)
              (store/commit! (:s x) {:parents [] :tree {:base nil} :subject (str "trace of " project) :body (str "root " root)}))))

;; --- a session's commits ---------------------------------------------------------------

(defn- project-of-state [st] (select-keys st [:project :root :common :git-dir]))

(defn- reroot
  "st with a root that is still there. A session's :root/:troot are where it started and last worked;
  when that worktree is removed under it, the project is found again from the event's cwd (same
  project: its fresh root, :troot/:thead dropped); when it cannot be, st stays and the commit keeps
  the previous tree (session-commit!). Never throws."
  [x st cwd]
  (try
    (let [live? #(boolean (and % (disk/project-of (:d x) %)))]
      (cond
        (not (live? (:root st)))
        (let [p (disk/project-of (:d x) cwd)]
          (if (and p (= (:project p) (:project st)))
            (-> st (merge (select-keys p [:root :common :git-dir])) (dissoc :troot :thead))
            st))
        (and (:troot st) (not= (:troot st) (:root st)) (not (live? (:troot st))))
        (dissoc st :troot :thead)
        :else st))
    (catch Exception _ st)))

(defn- turn-trailers
  "Model and token trailers from what the transcript adds (the usage is on the first call of a message)."
  [{:keys [model usage duration-ms]}]
  [["Duration-Ms" duration-ms] ["Model" model]
   ["Tokens-In" (:input_tokens usage)] ["Tokens-Out" (:output_tokens usage)]
   ["Cache-Read" (:cache_read_input_tokens usage)] ["Cache-Write" (:cache_creation_input_tokens usage)]])

(defn- session-commit!
  "A commit on the branch of st: the whole tree of the worktree at root now plus .claude-trace (keep?:
  the tree and Real-Head of the branch's previous commit, only .claude-trace changes); the real
  HEAD of that worktree is a trailer and is remembered as the branch's work on it. {:id :head}"
  [x st root subject body pairs ttree & [keep?]]
  (let [p (assoc (project-of-state st) :root root)
        parent (tip x (:branch st))
        snap0 (when-not keep? (disk/snapshot (:d x) p))
        keep? (or keep? (nil? snap0))             ; a worktree that is gone keeps the previous tree
        head (if keep? (or (:thead st) (disk/real-head (:d x) p)) (disk/real-head (:d x) p))
        snap (cond snap0 snap0 parent {:keep true} :else {:base nil})
        c (store/commit! (:s x) {:parents (vec (remove nil? [parent]))
                                 :tree (assoc snap :subtrees {".claude-trace" ttree})
                                 :subject subject
                                 :body (with-trailers body (concat pairs [["Real-Head" head]]))})]
    (note-head! x head (:branch st))
    (advance! x (:branch st) c)
    {:id c :head head}))

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

(defn- reports-of [x ids] (map #(vector "Agent-Report" (store/sha-of (:s x) %)) ids))

(defn- close!
  "Reply to the open prompt of key (a session id, or an agent's key)."
  [x key reply & [note? now-cwd]]
  (when-let [{:keys [branch nn open? reports cwd] :as st} (some-> (state x key) (as-> s (reroot x s (or now-cwd (:cwd s)))))]
    (when (and open? (tip x branch))
      (mirror! x (project-of-state st))
      (let [files (assoc (or (store/kv (:s x) :tfiles key) {}) "reply.md" reply)
            c (:id (session-commit! x st (or (:troot st) (:root st)) (str "response " nn) ""
                                    (concat (turn-trailers (:turn x)) (reports-of x reports) [["Cwd" cwd]])
                                    (store/tree! (:s x) files) true))]
        (when note? (note! x c))))
    (when st (state! x key (assoc st :open? false :reports [])))))

(defn on-prompt [x {:strs [session_id prompt cwd transcript_path]}]
  (close! x session_id "")
  (when-let [{:keys [project root common] :as p} (disk/project-of (:d x) cwd)]
    (mirror! x p)
    (ensure-main! x p)
    (let [branch (session-branch project session_id)
          raw (state x session_id)
          prev (some-> raw (as-> s (reroot x s cwd)))
          moved? (not= (select-keys raw [:root :troot]) (select-keys prev [:root :troot])) ; its worktree was gone
          n (inc (or (store/kv (:s x) :counters branch) -1))
          continuing? (and (pos? n) (= branch (:branch prev)) (tip x branch))
          troot (if continuing? (or (:troot prev) (:root prev)) root)
          nn (format "%02d" n)
          subject (str "prompt " nn ": " (clip prompt 100))
          reports (vec (keep #(take-pending! x %) (map second (re-seq #"<task-id>([A-Za-z0-9]+)</task-id>" (str prompt)))))
          files (assoc (disk/trace-files (:d x) session_id cwd transcript_path) "prompt.md" prompt)
          ttree (store/tree! (:s x) files)
          st {:project project :root root :troot troot :thead (when continuing? (:thead prev)) :common common :git-dir (:git-dir p) :branch branch :nn nn :k 0 :ttree ttree
              :open? true :cwd cwd :session session_id :subject subject :reports []
              :prompt-id (get-in x [:turn :prompt-id])}
          _ (when-not (tip x branch) (advance! x branch (tip x (main-branch project))))
          {c :id head :head} (session-commit! x st troot subject (str "session " session_id "\ncwd " cwd "\nroot " root)
                                              (concat [["Cwd" cwd] ["Prompt-Id" (get-in x [:turn :prompt-id])]] (reports-of x reports))
                                              ttree (and continuing? (not moved?)))]
      (note! x c)
      (when-let [out (some->> (re-find #"<output-file>(.+?)</output-file>" (str prompt)) second str/trim (disk/external-text (:d x)))]
        (store/note! (:s x) output-ref c out))
      (store/kv! (:s x) :counters branch n)
      (store/kv! (:s x) :tfiles session_id files)
      (state! x session_id (assoc st :thead head)))))

(defn- call-dir
  "Where a call demonstrably acted: the directory of the file it names, of a leading `cd X &&` or a
  `git -C X`, or the shell's cwd for a shell call. Nil for any other call (an Agent, a web fetch, an
  MCP tool without a path ...): it says nothing about a worktree."
  [{:strs [tool_input cwd]}]
  (let [abs (fn [d] (let [d (str/replace (str d) #"^['\"]|['\"]$" "")
                          d (if (str/starts-with? d "~") (str (fs/home) (subs d 1)) d)]
                      (cond (str/blank? d) nil
                            (str/starts-with? d "/") d
                            cwd (str (fs/path cwd d)))))]
    (or (when-let [f (some #(get tool_input %) ["file_path" "path" "notebook_path"])]
          (when (str/starts-with? (str f) "/") (str (fs/parent f))))
        (when-let [cmd (get tool_input "command")]
          (or (some-> (or (re-find #"^\s*cd\s+(\S+)\s*(?:&&|;)" cmd) (re-find #"git\s+-C\s+(\S+)" cmd)) second abs fs/normalize str)
              cwd)))))

(defn- call-root
  "The worktree a call acted in (see call-dir), nil when it did not demonstrably act in one: the
  session's own unless that is another worktree of the same project. Remembered per directory."
  [x st in]
  (let [dir (call-dir in)]
    (cond
      (str/blank? dir) nil
      (= dir (:root st)) (:root st)
      :else
      (let [k (store/blob-sha (str (:project st) " " dir))]
        (or (store/kv (:s x) :roots k)
            (let [p (loop [d dir n 0]
                      (or (disk/project-of (:d x) d)
                          (when (and (< n 6) (fs/parent d)) (recur (str (fs/parent d)) (inc n)))))
                  r (if (= (:project p) (:project st)) (:root p) (:root st))]
              (store/kv! (:s x) :roots k r)
              r))))))

(defn on-tool [x {:strs [session_id agent_id tool_name tool_input hook_event_name cwd] :as in}]
  (let [key (if agent_id (agent-key agent_id) session_id)]
    (when-let [{:keys [branch nn open? k] :as st} (some-> (state x key) (as-> s (reroot x s cwd)))]
      (when (and open? (tip x branch))
        (mirror! x (project-of-state st))
        (let [acted (call-root x st in)
              root (or acted (:troot st) (:root st))
              k (inc k)
              tool (short-tool tool_name)
              cmd (command-of in)
              text (text-of (get in "tool_response"))
              text (if (= hook_event_name "PostToolUseFailure") (or (get in "error") text) text)
              bad? (failed? in text)
              text (full-output x in text)
              file (the-file x root in)
              rel (some->> file (rel-of root))
              exit (some-> (re-find #"(?i)exit code:?\s*(\d+)" (subs text 0 (min 80 (count text)))) second)
              summary (if (and rel (not= cmd (get tool_input "command"))) rel (first-line cmd 70))
              {c :id head :head}
              (session-commit! x st root (format "%s.%d %s: %s" nn k tool summary) cmd
                               (concat [["Tool" tool] ["File" rel] ["Exit" exit]
                                        ["Error" (when bad? (first-line text 100))]]
                                       (turn-trailers (:turn x))
                                       [["Prompt-Id" (:prompt-id st)] ["Agent" (get in "agent_type")] ["Cwd" cwd]])
                               (:ttree st) (nil? acted))]
          (store/note! (:s x) output-ref c text)
          (note! x c)
          (state! x key (assoc st :k k :troot root :thead head)))))))

;; --- subagents ----------------------------------------------------------------------

(defn- brief-from-agent-file [session-id agent-id]
  (when-let [f (agents/find-file session-id agent-id ".jsonl")]
    (with-open [r (io/reader (str f))]
      (some-> (first (line-seq r)) (json/parse-string true) :message :content
              (#(if (string? %) % (str/join "\n" (keep :text %))))))))

(defn- brief-from-parent
  "What the parent asked: the prompt of its Agent tool_use, found in the tail of its transcript - the
  one the agent's meta.json names when that file is there yet, else the newest not taken by an agent."
  [x session-id agent-id transcript]
  (when (and transcript (fs/exists? transcript))
    (let [want (some-> (agents/find-file session-id agent-id ".meta.json") str slurp (json/parse-string true) :toolUseId)
          uses (for [e (rseq (turn/read-entries (:lines (probe/tail-lines transcript (* 8 1024 1024)))))
                     :when (= "assistant" (get e "type"))
                     b (get-in e ["message" "content"]) :when (and (= "tool_use" (get b "type")) (#{"Agent" "Task"} (get b "name")))]
                 b)
          use (if want
                (first (filter #(= want (get %1 "id")) uses))
                (first (remove #(store/kv (:s x) :agent-uses (get % "id")) uses)))]
      (when use
        (store/kv! (:s x) :agent-uses (get use "id") agent-id)
        (get-in use ["input" "prompt"])))))

(defn- agent-brief
  "The brief a subagent was given: the first message of its transcript when that is there yet,
  else the prompt of the parent's Agent call (the agent's file is written after SubagentStart)."
  [x session-id agent-id transcript]
  (or (try (brief-from-agent-file session-id agent-id) (catch Exception _ nil))
      (try (brief-from-parent x session-id agent-id transcript) (catch Exception _ nil))
      ""))

(defn on-subagent-start [x {:strs [session_id agent_id agent_type cwd transcript_path]}]
  (when-let [{:keys [project branch nn root common] :as st} (some-> (state x session_id) (as-> s (reroot x s cwd)))]
    (when-let [t (and agent_id (tip x branch))]
      (let [anid (str nn "-a" agent_id)
            ab (agent-branch project session_id agent_id)
            brief (agent-brief x session_id agent_id transcript_path)
            aroot (or (:troot st) (:root st))
            subject (str "prompt " anid ": " (clip (str agent_type ": " (first-line brief 80)) 100))
            files (assoc (disk/trace-files (:d x) session_id cwd nil) "prompt.md" brief)
            ttree (store/tree! (:s x) files)
            ast {:ttree ttree :project project :root aroot :troot aroot :thead (:thead st) :common common :git-dir (:git-dir st) :branch ab :nn anid :k 0 :open? true
                 :cwd cwd :session session_id :agent true :subject subject :reports []}]
        (mirror! x (project-of-state st))
        (advance! x ab t)
        (let [c (:id (session-commit! x ast aroot subject
                                 (str "session " session_id "\ncwd " cwd "\nroot " aroot "\nagent " agent_type
                                      "\nagent_id " agent_id "\nparent " branch)
                                 [["Cwd" cwd] ["Agent" agent_type]]
                                 ttree true))]
          (note! x c)
          (store/kv! (:s x) :tfiles (agent-key agent_id) files)
          (state! x (agent-key agent_id) ast))))))

(defn- agent-reply
  "What an agent said last: the payload's message, else the last assistant text of its transcript."
  [session-id agent-id message]
  (or (when-not (str/blank? message) message)
      (when-let [f (agents/find-file session-id agent-id ".jsonl")]
        (turn/last-text (turn/read-entries (:lines (probe/tail-lines f (* 256 1024))))))
      ""))

(defn on-subagent-stop
  "Close the agent's branch. The parent hears of it where it reacts to the agent's notification:
  in the turn that is open (the notification is delivered into it), else in the prompt that
  the notification opens; either way as an Agent-Report trailer, not a parent link."
  [x {:strs [session_id agent_id last_assistant_message]}]
  (when agent_id
    (let [b (:branch (state x (agent-key agent_id)))]
      (close! x (agent-key agent_id) (agent-reply session_id agent_id last_assistant_message) true)
      (when-let [t (and b (tip x b))]
        (let [st (state x session_id)]
          (if (:open? st)
            (state! x session_id (update st :reports (fnil conj []) t))
            (pending! x agent_id {:tip t :session session_id})))))))

(defn- agent-states-of [x session-id]
  (keep (fn [[k st]]
          (when (and (:agent st) (:open? st) (= session-id (:session st))) (str/replace k #"^agent-" "")))
        (store/kv-all (:s x) :sessions)))

(defn on-stop [x {:strs [session_id hook_event_name cwd] :as in}]
  (close! x session_id (reply-of in) true cwd)
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
     "tool" (:open? (state x (if agent_id (agent-key agent_id) session_id)))
     "prompt" (disk/project-of (:d x) cwd)
     "stop" (:open? (state x session_id))
     "subagent-start" (state x session_id)
     "subagent-stop" (and agent_id (:open? (state x (agent-key agent_id))))
     nil)))

(defn- payload-turn
  "Model and usage a payload brings itself when there is no transcript to read them from (moot
  records a deepseek exchange that way); usage has the transcript's key names."
  [{:strs [model usage transcript_path]}]
  (when (and (nil? transcript_path) (or model usage))
    {:model model :usage (update-keys (or usage {}) keyword)}))

(defn handle-in!
  "Handle one hook event in context x ({:s store :d disk :probe? bool}); raw is its stdin as
  received (default: the parsed input re-encoded)."
  [x event input & [raw]]
  (store/ensure! (:s x))
  (let [x (assoc x :raw (or raw (json/generate-string input)))
        live? (and (:probe? x) (recorded? x event input))
        x (assoc x :probed (when live? (probe-event x event input))
                 :turn (or (if live? (live-turn event input) (:turn x)) (payload-turn input))
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
