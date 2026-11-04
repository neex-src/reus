(ns trace.record
  "Every Claude session on this machine becomes git history in one trace repo (trace.git/*repo*),
  nothing squashed. The hook (trace.hook) only observes (trace.observe: the worktree's tree, hashed
  before Claude goes on) and spools; this ns makes the history from an event and its observation,
  in the worker (trace.worker) for the live hook, at once for a replay or a test (handle!).

  Refs. A project is a repo (named after its main checkout's dir, also for a worktree of it; a
  second repo with the same dir name gets <name>-<6 hex>); outside git, the <projects-root>/<name>
  dir it sits in; elsewhere nothing is traced.
    <project>/main                 the real main, commit for commit
    <project>/s/<session8>         one Claude session's work in that project
    <project>/a/<session8>-<agent> one subagent run's work in that project
  (a ref cannot be `x` and `x/y` at once, hence the prefixes).

  A session (or agent) branch: `prompt NN: ...`, one commit per tool call (`NN.k Tool: ...`),
  `response NN`, then the next prompt on the same branch; NN counts the session's prompts, k its
  calls in the prompt. A call that acts in another project is committed on that project's branch of
  the session, which gets the `prompt NN` commit first (so every project shows the prompt its calls
  answer); the response closes every branch the prompt touched. Every commit's tree is the whole
  tree of the worktree the call acted in, right after the call (tracked and untracked files that are
  not ignored, as `git add -A` has them: so a commit's diff is what that call changed, and a read is
  an empty diff); a call that acts in no worktree keeps the previous tree. Plus .claude-trace/ with
  prompt.md, reply.md, memory/ (the project's memory files) and claude-md/ (the CLAUDE.md chain in
  effect), snapshotted at each prompt. Outside git nothing is hashed (a dir can be anything). The
  trace repo holds every object it refers to (no alternates): a snapshot writes the blobs it hashes,
  a mirror commit copies the real commit's tree from the project repo.

  Commit messages end in trailers: Tool, File, Exit, Error, Duration-Ms, Model, Tokens-In,
  Tokens-Out, Cache-Read, Cache-Write, Prompt-Id, Agent, Cwd, Real-Head (the worktree's HEAD at that
  moment), Worktree (the root of the worktree the tree is of: trace.fns keeps a line per worktree),
  Agent-Report (the tip of an agent whose report this turn got, which is no parent link), Snapshot-Error (the worktree could not be hashed: the tree is the previous one). Model and tokens
  come from the transcript; a payload without one may bring them as `model` and `usage` (moot feeds
  a deepseek exchange as UserPromptSubmit / Stop with last_assistant_message). Notes:
  refs/notes/event is the hook stdin verbatim, turn what the transcript adds (trace.turn), output the
  call's output text (a tool output Claude Code persisted to a file is recorded in full) or the
  output file of a background task a prompt's notification names. (refs/notes/probe: older traces
  only, see trace.probe.)

  Trace main mirrors the real main's first-parent history: a commit per real commit, same tree, the
  real message and author plus `Real-Commit: <sha>`. A real merge gets as further parents the tips of
  the session and agent branches that did work in the commits it merged (those whose Real-Head is
  among them), so `git log -p <project>/main -- <file>` shows every tool call that led to the file.
  The mirror catches up whenever an event of the project is recorded; it never fails the event.

  A subagent run is recorded like a session of its own: SubagentStart opens its branch, cut from the
  parent's tip, with `prompt NN-a<id>: ...` holding the agent's brief; every tool event that carries
  an agent_id goes to the agent (opened on the spot when its start was never seen); SubagentStop adds
  `response NN-a<id>`. SessionEnd closes agents still open.

  Commits are built with commit-tree and moved with update-ref, under the store's write lock.
  Session state is in .git/trace-sessions/<session id or agent-<id>>."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [trace.agents :as agents]
            [trace.disk :as disk]
            [trace.files :as files]
            [trace.git :as g]
            [trace.observe :as observe]
            [trace.probe :as probe]
            [trace.turn :as turn]
            [trace.store :as store]))

(def edit-tools #{"Edit" "MultiEdit" "Write" "NotebookEdit" "file_edit" "file_write" "clojure_edit" "clojure_edit_replace_sexp"})
(def read-tools #{"Read" "NotebookRead" "read_file"})

(def notes-ref "refs/notes/event")
(def probe-ref "refs/notes/probe")
(def turn-ref "refs/notes/turn")
(def output-ref "refs/notes/output")

;; x is the context of one event: {:s store :d disk :raw payload as received :obs observation
;; :turn what the transcript adds :tkey the key of the session or agent :live? read the transcript}

(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "...") s)))
(defn- first-line [s n] (clip (first (str/split-lines (str s))) n))
(defn- short-tool [t] (str/replace (str t) #"^mcp__.*__" ""))
(defn- not-blank [s] (when-not (str/blank? s) s))

(defn ensure-repo! [] (store/ensure! (store/live g/*repo*)))

(defn project-of
  "{:project name :root dir} for a session cwd, or nil when it is not traced."
  ([cwd] (project-of (disk/live g/*repo*) cwd))
  ([d cwd] (disk/project-of d cwd)))

(defn- warn!
  "A failure that must not fail the event (the mirror): one line in .git/trace-errors.log."
  [x what e]
  (try (spit (str (fs/path (:repo (:s x)) ".git" "trace-errors.log"))
             (str (java.time.Instant/now) " " what " " (:tkey x) " " (or (ex-message e) (str e)) "\n") :append true)
       (catch Throwable _)))

;; --- the store, by branch -----------------------------------------------------------

(defn- head-ref [branch] (str "refs/heads/" branch))
(defn- tip [x branch] (store/tip (:s x) (head-ref branch)))
(defn- advance! [x branch id] (store/advance! (:s x) (head-ref branch) id))

(defn- notes!
  "Keep the event's raw payload, what the transcript adds and more {ref content} as notes on commit c."
  [x c more]
  (let [t (:turn x)
        m (cond-> (into {} (remove (comp nil? val) more))
            (:raw x) (assoc notes-ref (:raw x))
            t (assoc turn-ref (pr-str (dissoc t :pending-id))))]
    (store/notes! (:s x) c m)
    (when-let [id (:pending-id t)] ; the tool_use was not in the transcript yet: looked for at the next event
      (store/kv! (:s x) :turn-pending (:tkey x) {:id id :commit (store/sha-of (:s x) c)}))))

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

;; --- sessions and agents (actors) ---------------------------------------------------

(defn- agent-key [agent-id] (str "agent-" agent-id))
(defn- s8 [session-id] (subs (str session-id) 0 (min 8 (count (str session-id)))))
(defn- main-branch [project] (str project "/main"))
(defn- session-branch [project session-id] (str project "/s/" (s8 session-id)))
(defn- agent-branch [project session-id agent-id] (str project "/a/" (s8 session-id) "-" agent-id))
(defn- tkey-of [{:strs [agent_id session_id]}] (if agent_id (agent-key agent_id) session_id))

(defn- branch-of [a project]
  (if (:agent a) (agent-branch project (:session a) (:agent a)) (session-branch project (:session a))))

(defn- upgrade
  "An actor's state as this ns keeps it ({:v 2 ...}, see actor) from what older code kept: one
  branch, its root and HEAD, prompts counted per branch."
  [x key st]
  (if (or (nil? st) (:v st))
    st
    (let [{:keys [project branch root troot thead common git-dir nn]} st
          agent (when (str/starts-with? (str key) "agent-") (subs key 6))]
      (cond-> (merge (select-keys st [:session :nn :k :open? :ttree :subject :reports :prompt-id :cwd])
                     {:v 2 :agent agent :parent (when agent (:session st)) :current project
                      :n (or (store/kv (:s x) :counters branch) (some-> nn (str/split #"-") first parse-long) -1)
                      :branches (if project
                                  {project {:project project :branch branch :root (or troot root) :common common
                                            :git-dir git-dir :thead thead :nn nn}}
                                  {})})
        (nil? (:k st)) (assoc :k 0)))))

(defn- actor
  "The state of a session or an agent:
  {:v 2 :session :agent (id, agents) :agent-type :n (prompts so far) :nn :k (calls in the prompt)
   :open? :subject :ttree :prompt-id :cwd :reports [agent tips]
   :branches {project {:project :branch :root :common :git-dir :thead :nn (its latest prompt)}}
   :current project (where it last committed)}"
  [x key] (upgrade x key (store/kv (:s x) :sessions key)))
(defn- actor!
  "Keep a's state; the actor whose event this is also keeps the spool entry it came from (:last-entry),
  in the same write, so a worker cut off after it does not record that event again."
  [x key a]
  (store/kv! (:s x) :sessions key (cond-> a (and (:entry x) (= key (:tkey x))) (assoc :last-entry (:entry x)))))

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
  (when-let [{:keys [tree parents author committer message]} (disk/commit-info (:d x) p sha)]
    (let [merged (when (> (count parents) 1)
                   (distinct (mapcat (fn [q] (mapcat #(store/kv (:s x) :realheads %) (disk/range-set (:d x) p (first parents) q)))
                                     (rest parents))))
          extra (keep #(tip x %) merged)
          parent (store/tip (:s x) ref)
          msg (add-trailer message "Real-Commit" sha)
          [subject body] (let [i (str/index-of msg "\n")] (if i [(subs msg 0 i) (str/triml (subs msg i))] [msg ""]))]
      (store/advance! (:s x) ref
                      (store/commit! (:s x) {:parents (vec (concat (when parent [parent]) extra)) :tree {:base tree :from (:common p)}
                                             :subject subject :body body :author author :committer committer})))))

(def mirror-retry-ms "A mirror that failed is tried again for the same real tip after this long." (* 10 60 1000))

(defn- mirror!
  "Bring trace main up to the real main: one commit per real first-parent commit since the last one
  mirrored (the first time, or when that one is gone from the repo, just the tip); the last mirrored
  is remembered after every commit, so a cut-off catch-up resumes instead of repeating. Never throws."
  [x {:keys [project common] :as p}]
  (when common
    (let [tip* (disk/main-tip (:d x) p)
          known (store/kv (:s x) :mirror project)]
      (when (and tip* (not= tip* known)
                 (not (let [{:keys [tip at]} (store/kv (:s x) :mirror-failed project)] ; a failing mirror waits a while
                        (and (= tip tip*) (< (- (System/currentTimeMillis) (or at 0)) mirror-retry-ms)))))
        (try
          (let [ref (head-ref (main-branch project))
                ;; the trace main tip says what it mirrors: a commit made right before a cut-off is not made again
                last* (or (some-> (store/tip (:s x) ref) (as-> t (store/trailer (:s x) t "Real-Commit"))) known)]
            (when (not= tip* last*)
              (doseq [sha (disk/chain (:d x) p last* tip*)]
                (mirror-commit! x p ref sha)
                (store/kv! (:s x) :mirror project sha)))
            (store/kv! (:s x) :mirror project tip*)
            (store/kv! (:s x) :mirror-failed project nil))
          (catch Exception e
            ;; not again for this tip for a while: the next real commit tries at once
            (store/kv! (:s x) :mirror-failed project {:tip tip* :at (System/currentTimeMillis)})
            (warn! x (str "mirror " project) e)))))))

(defn- ensure-main!
  "A project without a real main (or outside git) gets an empty root on trace main."
  [x {:keys [project root]}]
  (when-not (tip x (main-branch project))
    (advance! x (main-branch project)
              (store/commit! (:s x) {:parents [] :tree {:base nil} :subject (str "trace of " project) :body (str "root " root)}))))

;; --- commits ----------------------------------------------------------------------------

(defn- turn-trailers
  "Model and token trailers from what the transcript adds (the usage is on the first call of a message)."
  [{:keys [model usage duration-ms]}]
  [["Duration-Ms" duration-ms] ["Model" model]
   ["Tokens-In" (:input_tokens usage)] ["Tokens-Out" (:output_tokens usage)]
   ["Cache-Read" (:cache_read_input_tokens usage)] ["Cache-Write" (:cache_creation_input_tokens usage)]])

(defn- commit-on!
  "A commit on branch: the tree spec snap (a worktree's snapshot; nil: the branch's previous tree)
  with .claude-trace ttree; head is its Real-Head and is remembered as the branch's work on it, root
  the worktree that tree is of (Worktree)."
  [x branch snap head root ttree subject body pairs]
  (let [parent (tip x branch)
        spec (cond snap snap parent {:keep true} :else {:base nil})
        c (store/commit! (:s x) {:parents (vec (remove nil? [parent]))
                                 :tree (assoc spec :subtrees {".claude-trace" ttree})
                                 :subject subject
                                 :body (with-trailers body (concat pairs [["Real-Head" head] ["Worktree" root]]))})]
    (note-head! x head branch)
    (advance! x branch c)
    c))

(defn- reports-of [x ids] (map #(vector "Agent-Report" (store/sha-of (:s x) %)) ids))

(defn- enter!
  "a with its branch in project p (a worktree map) carrying the current prompt's commit: made when
  the branch does not have it yet (cut from the parent session's branch in p for an agent, else
  from trace main), its tree the worktree's snapshot snap when given, else the branch's previous.
  [a commit-or-nil]."
  [x a p {:keys [snap head body pairs]}]
  (let [project (:project p)
        b (get-in a [:branches project])
        branch (or (:branch b) (branch-of a project))]
    (if (and b (= (:nn b) (:nn a)) (tip x branch))
      [a nil]
      (do
        (mirror! x p)
        (ensure-main! x p)
        (when-not (tip x branch)
          (let [parent-b (when-let [pk (:parent a)] (get-in (actor x pk) [:branches project :branch]))]
            (advance! x branch (or (some->> parent-b (tip x)) (tip x (main-branch project))))))
        (let [head (or head (:thead b))
              c (commit-on! x branch snap head (if snap (:root p) (or (:root b) (:root p))) (:ttree a) (:subject a)
                            (or body (str "session " (:session a) "\ncwd " (:cwd a) "\nroot " (:root p)))
                            (or pairs [["Cwd" (:cwd a)] ["Prompt-Id" (:prompt-id a)] ["Agent" (:agent-type a)]]))]
          [(assoc-in a [:branches project]
                     (merge (select-keys p [:project :root :common :git-dir])
                            {:branch branch :thead head :nn (:nn a)}))
           c])))))

(defn- worktree-of-branch
  "The worktree map of a branch entry (what enter! and the mirror need)."
  [b] (select-keys b [:project :root :common :git-dir]))

(defn- close!
  "Reply to the open prompt of key (a session id, or an agent's key): a response on every branch the
  prompt touched; the one it ended in gets the turn's trailers, the agent reports and the notes."
  [x k* reply note?]
  (when-let [a (actor x k*)]
    (when (:open? a)
      (let [touched (filter #(and (= (:nn a) (:nn (val %))) (tip x (:branch (val %)))) (:branches a))
            reply (force reply)]
        (when (seq touched)
          (let [ttree (store/tree! (:s x) (assoc (or (store/kv (:s x) :tfiles k*) {}) "reply.md" reply))
                cur (if (some #(= (:current a) (key %)) touched) (:current a) (key (first touched)))]
            (doseq [[project b] touched
                    :let [cur? (= project cur)]]
              (mirror! x (worktree-of-branch b))
              (let [c (commit-on! x (:branch b) nil (:thead b) (:root b) ttree (str "response " (:nn a)) ""
                                  (concat (when cur? (turn-trailers (:turn x)))
                                          (when cur? (reports-of x (:reports a)))
                                          [["Cwd" (:cwd a)]]))]
                (when (and note? cur?) (notes! x c nil)))))))
      (actor! x k* (assoc a :open? false :reports [])))))

;; --- prompts ------------------------------------------------------------------------------

(defn- snap-pairs [obs]
  [["Snapshot-Error" (:snap-error obs)] ["Observe-Error" (:error obs)]])

(defn on-prompt [x {:strs [session_id prompt cwd]}]
  (close! x session_id "" false)
  (let [obs (:obs x)
        prev (actor x session_id)
        n (inc (or (:n prev) -1))
        nn (format "%02d" n)
        reports (vec (keep #(take-pending! x %) (map second (re-seq #"<task-id>([A-Za-z0-9]+)</task-id>" (str prompt)))))
        files (assoc (or (:files obs) {}) "prompt.md" prompt)
        a (merge (or prev {:v 2 :session session_id :branches {}})
                 {:v 2 :session session_id :n n :nn nn :k 0 :open? true
                  :subject (str "prompt " nn ": " (clip prompt 100))
                  :ttree (store/tree! (:s x) files) :prompt-id (get-in x [:turn :prompt-id]) :cwd cwd :reports []})]
    (store/kv! (:s x) :tfiles session_id files)
    (if-let [p (:wt obs)]
      (let [[a c] (enter! x a p {:snap (:snap p) :head (:head p)
                                 :body (str "session " session_id "\ncwd " cwd "\nroot " (:root p))
                                 :pairs (concat [["Cwd" cwd] ["Prompt-Id" (:prompt-id a)]] (reports-of x reports) (snap-pairs obs))})]
        (when c
          (notes! x c {output-ref (:output obs)}))
        (actor! x session_id (assoc a :current (:project p))))
      (actor! x session_id (assoc a :reports reports)))))

;; --- tool calls -----------------------------------------------------------------------------

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
    (if (and root (str/starts-with? c (str root "/"))) (subs c (inc (count root))) (str "abs" c))))

(defn- brief-from-agent-file [session-id agent-id transcript]
  (when-let [f (agents/find-file session-id agent-id ".jsonl" transcript)]
    (with-open [r (io/reader (str f))]
      (some-> (first (line-seq r)) (json/parse-string true) :message :content
              (#(if (string? %) % (str/join "\n" (keep :text %))))))))

(defn- new-agent
  "The state of an agent run of session-id (parent: the session's state, or nil)."
  [x parent {:strs [session_id agent_id agent_type cwd]} brief]
  (let [nn (str (or (:nn parent) "00") "-a" agent_id)
        files (assoc (or (get-in x [:obs :files]) (store/kv (:s x) :tfiles session_id) {}) "prompt.md" brief)]
    (store/kv! (:s x) :tfiles (agent-key agent_id) files)
    {:v 2 :session session_id :agent agent_id :agent-type agent_type :parent session_id
     :n 0 :nn nn :k 0 :open? true :cwd cwd :reports [] :branches {}
     :subject (str "prompt " nn ": " (clip (str agent_type ": " (first-line brief 80)) 100))
     :ttree (store/tree! (:s x) files)}))

(defn on-tool [x {:strs [session_id agent_id tool_name tool_input hook_event_name cwd transcript_path] :as in}]
  (let [key (tkey-of in)
        a (or (actor x key)
              (when agent_id ; its SubagentStart was never recorded: the agent starts here
                (new-agent x (actor x session_id) in
                           (or (try (brief-from-agent-file session_id agent_id transcript_path) (catch Exception _ nil))
                               "(the agent's start was not recorded)"))))
        obs (:obs x)
        wt (:wt obs)]
    (when (and a (:open? a))
      (when-let [p (or wt (get-in a [:branches (:current a)]))]
        (mirror! x p)
        (let [[a _] (enter! x a p {})
              b (get-in a [:branches (:project p)])
              k (inc (:k a))
              head (if wt (:head wt) (:thead b))
              tool (short-tool tool_name)
              cmd (command-of in)
              text (observe/text-of (get in "tool_response"))
              text (if (= hook_event_name "PostToolUseFailure") (or (get in "error") text) text)
              bad? (failed? in text)
              text (or (:output obs) text)
              rel (some->> (:file obs) (rel-of (:root p)))
              exit (some-> (re-find #"(?i)exit code:?\s*(\d+)" (subs text 0 (min 80 (count text)))) second)
              summary (if (and rel (not= cmd (get tool_input "command"))) rel (first-line cmd 70))
              c (commit-on! x (:branch b) (:snap wt) head (if wt (:root wt) (:root b)) (:ttree a) (format "%s.%d %s: %s" (:nn a) k tool summary) cmd
                            (concat [["Tool" tool] ["File" rel] ["Exit" exit]
                                     ["Error" (when bad? (first-line text 100))]]
                                    (turn-trailers (:turn x))
                                    [["Prompt-Id" (:prompt-id a)] ["Agent" (get in "agent_type")] ["Cwd" cwd]]
                                    (snap-pairs obs)))]
          (notes! x c {output-ref text})
          (actor! x key (-> a
                            (assoc :k k :current (:project p))
                            (update-in [:branches (:project p)] merge
                                       (cond-> {:thead head} wt (merge (select-keys wt [:root :common :git-dir])))))))))))

;; --- subagents ----------------------------------------------------------------------

(defn- brief-from-parent
  "What the parent asked: the prompt of its Agent tool_use, found in the tail of its transcript - the
  one the agent's meta.json names when that file is there yet, else the newest not taken by an agent."
  [x session-id agent-id transcript]
  (when (and transcript (fs/exists? transcript))
    (let [want (some-> (agents/find-file session-id agent-id ".meta.json" transcript) str slurp (json/parse-string true) :toolUseId)
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

(def brief-wait-ms
  "How long the worker waits for an agent's brief to reach a transcript: Claude Code writes the agent's
  file after SubagentStart returns, and a warm worker gets to the event within milliseconds."
  5000)

(defn- agent-brief
  "The brief a subagent was given: the first message of its transcript, else the prompt of the parent's
  Agent call, as soon as either is written (at most brief-wait-ms)."
  [x session-id agent-id transcript]
  (let [deadline (+ (System/currentTimeMillis) brief-wait-ms)]
    (loop []
      (or (not-blank (try (brief-from-agent-file session-id agent-id transcript) (catch Exception _ nil)))
          (not-blank (try (brief-from-parent x session-id agent-id transcript) (catch Exception _ nil)))
          (if (and (:live? x) transcript (fs/exists? transcript) (< (System/currentTimeMillis) deadline)) ; a real session
            (do (Thread/sleep 100) (recur))
            "")))))

(defn on-subagent-start [x {:strs [session_id agent_id agent_type cwd transcript_path] :as in}]
  (when agent_id
    (let [parent (actor x session_id)
          a (new-agent x parent in (agent-brief x session_id agent_id transcript_path))
          obs (:obs x)
          p (or (get-in parent [:branches (:current parent)]) (:cwd-project obs))]
      (if p
        (let [[a c] (enter! x a p {:head (:thead p)
                                   :body (str "session " session_id "\ncwd " cwd "\nroot " (:root p) "\nagent " agent_type
                                              "\nagent_id " agent_id "\nparent " (get-in parent [:branches (:project p) :branch]))
                                   :pairs (concat [["Cwd" cwd] ["Agent" agent_type]] (snap-pairs obs))})]
          (when c (notes! x c nil))
          (actor! x (agent-key agent_id) (assoc a :current (:project p))))
        (actor! x (agent-key agent_id) a)))))

(defn- agent-reply
  "What an agent said last: the payload's message, else the last assistant text of its transcript
  (SubagentStop names it, agent_transcript_path)."
  [session-id agent-id message transcript agent-transcript]
  (or (not-blank message)
      (when-let [f (or (when (some-> agent-transcript fs/exists?) agent-transcript)
                       (agents/find-file session-id agent-id ".jsonl" transcript))]
        (turn/last-text (turn/read-entries (:lines (probe/tail-lines f (* 256 1024))))))
      ""))

(defn on-subagent-stop
  "Close the agent's branches. The parent hears of it where it reacts to the agent's notification:
  in the turn that is open (the notification is delivered into it), else in the prompt that the
  notification opens; either way as an Agent-Report trailer, not a parent link."
  [x {:strs [session_id agent_id last_assistant_message transcript_path agent_transcript_path]}]
  (when agent_id
    (let [a (actor x (agent-key agent_id))]
      (close! x (agent-key agent_id) (delay (agent-reply session_id agent_id last_assistant_message transcript_path agent_transcript_path)) true)
      (when-let [t (some->> (get-in a [:branches (:current a) :branch]) (tip x))]
        (let [st (actor x session_id)]
          (if (:open? st)
            (actor! x session_id (update st :reports (fnil conj []) t))
            (pending! x agent_id {:tip (store/sha-of (:s x) t) :session session_id})))))))

(defn- agents-of [x session-id]
  (keep (fn [[k st]]
          (when (and (str/starts-with? k "agent-") (:open? st) (= session-id (:session st))) (subs k 6)))
        (store/kv-all (:s x) :sessions)))

;; --- stops ----------------------------------------------------------------------------------

(defn- tail-text
  "The last assistant text in the transcript as it was `size` bytes long (nil: now)."
  [file size]
  (when (and file (fs/exists? file))
    (turn/last-text (turn/read-entries (:lines (probe/tail-lines file (* 256 1024) size))))))

(defn- reply-of [x {:strs [last_assistant_message transcript_path]}]
  (or (not-blank last_assistant_message)
      (try (tail-text transcript_path (get-in x [:obs :tsize])) (catch Exception _ nil))
      ""))

(defn on-stop [x {:strs [session_id hook_event_name] :as in}]
  (close! x session_id (delay (reply-of x in)) true)
  (when (= "SessionEnd" hook_event_name)
    (doseq [a (agents-of x session_id)] (close! x (agent-key a) "" false))))

;; --- what the transcript adds -----------------------------------------------------------------

(defn- transcript-of [{:strs [transcript_path agent_id session_id]}]
  (if agent_id (some-> (agents/find-file session_id agent_id ".jsonl" transcript_path) str) transcript_path))

(defn- tail-index
  "The index of the tail of a transcript (the last `window` bytes, of its first `size` bytes when given)."
  [file window & [size]]
  (turn/index (turn/read-entries (:lines (probe/tail-lines file window size)))))

(def ^:private windows [(* 256 1024) (* 8 1024 1024)])

(defn- live-turn
  "What the transcript adds to an event (trace.turn), from its tail; nil when it is not there. A
  stop is read as the transcript was when its hook fired (a new prompt may have come since). A tool
  call whose tool_use is not there yet is {:pending-id id}, found at a later event."
  [event {:strs [tool_use_id prompt] :as input} obs]
  (try
    (let [file (transcript-of input)]
      (when (and file (fs/exists? file))
        (case event
          "tool" (or (some #(turn/tool (tail-index file %) tool_use_id {:duration-ms (get input "duration_ms")}) windows)
                     {:use-found? false :pending-id tool_use_id})
          "prompt" (let [ix (tail-index file (first windows))]
                     (some->> (turn/find-prompt ix prompt) (turn/prompt ix)))
          "stop" (turn/stop (tail-index file (first windows) (:tsize obs)))
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

(defn- payload-turn
  "Model and usage a payload brings itself when there is no transcript to read them from (moot
  records a deepseek exchange that way); usage has the transcript's key names."
  [{:strs [model usage duration_ms transcript_path]}]
  (when (and (nil? transcript_path) (or model usage duration_ms))
    (cond-> {:model model :usage (update-keys (or usage {}) keyword)}
      duration_ms (assoc :duration-ms duration_ms))))

;; --- events ---------------------------------------------------------------------------------

(defn observe-now
  "The observation of an event made now, through the store's own state (a replay, a test, moot):
  the live hook makes it in its own process (trace.hook) the same way."
  [x event input]
  (let [s (:s x)
        key (tkey-of input)
        lock (fn [f] (if (:proc s) (f) (files/with-file-lock (files/projects-lock (:repo s)) 5000 f f))) ; a batch is alone
        name-of (observe/project-namer #(store/kv s :projects %) #(store/kv! s :projects %1 %2))
        obs (observe/observe (:d x) event input {:last-root (store/kv s :hookroot key) :name-of #(lock (fn [] (name-of %)))})]
    (when-let [r (and (#{"tool" "prompt" "subagent-start"} event) (get-in obs [:wt :root]))]
      (store/kv! s :hookroot key r))
    obs))

(defn handle-in!
  "Record one hook event in context x ({:s store :d disk, :live? read the transcript}): input the
  parsed payload, raw its stdin as received (default: input re-encoded), obs its observation
  (trace.observe; made now when not given), entry the spool entry it comes from (an entry its
  actor's state names as recorded, or a later one, is skipped)."
  [x event input & [{:keys [raw obs entry]}]]
  (store/ensure! (:s x))
  (let [obs (or obs (observe-now x event input))
        x (assoc x :raw (or raw (json/generate-string input)) :obs obs :tkey (tkey-of input) :entry entry)
        x (assoc x :turn (or (:turn x) (when (:live? x) (live-turn event input obs)) (payload-turn input)))
        late (when (and (:live? x) (#{"tool" "stop"} event)) (late-turn x (:tkey x) input))]
    (store/locked
     (:s x)
     #(when-not (when-let [e (:entry x)] ; recorded already, by a worker cut off before it could say so
                  (when-let [l (:last-entry (store/kv (:s x) :sessions (:tkey x)))] (not (neg? (compare l e)))))
        (when late
            (store/note! (:s x) turn-ref (:commit late) (pr-str (:turn late)))
            (store/kv! (:s x) :turn-pending (:tkey x) nil))
          (case event
            "prompt" (on-prompt x input)
            "tool" (on-tool x input)
            "stop" (on-stop x input)
            "subagent-start" (on-subagent-start x input)
            "subagent-stop" (on-subagent-stop x input)
            nil)))))

(defn handle!
  "Record one hook event at once, in the trace repo g/*repo* (tests, moot): observed and recorded in
  this process, as the hook and the worker do it in two."
  [event input & [raw]]
  (handle-in! {:s (store/live g/*repo*) :d (disk/live g/*repo*) :live? true} event input {:raw raw}))

(defn -main
  "The hook command of older installs (`-m trace.record <event>`): the same as trace.hook's."
  [& args]
  (apply (requiring-resolve 'trace.hook/-main) args))
