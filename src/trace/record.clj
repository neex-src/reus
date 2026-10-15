(ns trace.record
  "Global Claude Code hook: every session on this machine becomes git history in one
  trace repo (trace.git/*repo*), nothing squashed.

  Hook command:  bb --config <reus>/bb.edn -m trace.record <event>
    prompt  UserPromptSubmit
    tool    PostToolUse / PostToolUseFailure
    stop    Stop, SessionEnd

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

  Nothing is ever checked out: commits are built with a private index and
  commit-tree and moved with update-ref, under one mkdir lock, so concurrent
  sessions cannot disturb each other. The open prompt of a session is
  .git/trace-sessions/<session id>. Errors go to .git/trace-errors.log; the hook
  always exits 0, so tracing never breaks a session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.git :as g :refer [git]]))

(def delim "--- error ---")
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

(defn- tip [branch]
  (let [r (g/git* {} "rev-parse" "--verify" "-q" (str "refs/heads/" branch))]
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

(defn- advance! [branch sha] (git "update-ref" (str "refs/heads/" branch) sha))
(defn- has? [rev path] (zero? (:exit (g/git* {} "cat-file" "-e" (str rev ":" path)))))

;; --- sessions -----------------------------------------------------------------

(defn- state-file [sid] (gitdir "trace-sessions" (str/replace (str sid) #"[^A-Za-z0-9._-]" "_")))
(defn- state [sid] (let [f (state-file sid)] (when (fs/exists? f) (edn/read-string (slurp f)))))
(defn- state! [sid m]
  (let [f (state-file sid)]
    (if m (do (fs/create-dirs (fs/parent f)) (spit f (pr-str m))) (fs/delete-if-exists f))))

(defn project-of
  "{:project name :root dir} for a session cwd, or nil when it is not traced."
  [cwd]
  (when (and cwd (fs/directory? cwd))
    (let [cwd (str (fs/canonicalize cwd))
          r (p/shell {:out :string :err :string :continue true :dir cwd}
                     "git" "rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel")
          projects (str (fs/path (fs/home) "projects"))
          clean #(str/replace % #"[^A-Za-z0-9._-]" "_")]
      (cond
        (str/starts-with? (str cwd "/") (str (fs/canonicalize (fs/create-dirs g/*repo*)) "/")) nil
        (zero? (:exit r))
        (let [[common top] (str/split-lines (str/trim (:out r)))
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
  (let [c (str (fs/canonicalize f))]
    (if (str/starts-with? c (str root "/")) (subs c (inc (count root))) (str "abs" c))))

(defn- the-file
  "The one existing regular file this call is about: its file_path/path/notebook_path,
  else the one existing file of the root a shell command names."
  [root {:strs [tool_input cwd]}]
  (let [resolve (fn [tok]
                  (let [tok (str/replace (str tok) #"^['\"]|['\"]$" "")
                        f (cond (str/blank? tok) nil
                                (fs/absolute? tok) tok
                                :else (some #(let [c (str (fs/path % tok))] (when (fs/exists? c) c)) [(or cwd root) root]))]
                    (when (and f (fs/regular-file? f)) (str (fs/canonicalize f)))))]
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
      (let [r (p/shell {:out :string :err :string :continue true :dir root} "git" "show" (str "HEAD:" rel))]
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
  "Reply to the session's open prompt and merge it into its project (--no-ff: the
  merge's tree is the prompt's)."
  [sid reply]
  (when-let [{:keys [project branch nn subject]} (state sid)]
    (when-let [t (tip branch)]
      (let [r (commit! t {"doto.md" reply} (str "response " nn) "")]
        (advance! branch r)
        (advance! project (git "commit-tree" (str r "^{tree}") "-p" (tip project) "-p" r "-m" subject))))
    (state! sid nil)))

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
      (state! session_id {:project project :branch branch :nn nn :root root :k 0 :subject subject}))))

(defn on-tool [{:strs [session_id tool_name tool_input hook_event_name] :as in}]
  (when-let [{:keys [branch nn root k] :as st} (state session_id)]
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
                (let [now (slurp file)
                      path (str "files/" rel)
                      t (if (has? t path)
                          t
                          (commit! t {path (before-edit root rel now in)} (str "baseline " rel)
                                   "the file before its first traced edit"))]
                  (commit! t {path now} (str tool ": " (first-line rel 90)) (str cmd agent)))

                (and file (or (read-tools tool) (get tool_input "command")))
                (commit! t {(str "reads/" rel) (if bad? (str delim "\n" text) text)}
                         (git "hash-object" "-w" file) (str cmd agent))

                :else
                (commit! t {(format "reads/_cmd/%s.%d" nn k) (if bad? (str delim "\n" text) text)}
                         (format "%s.%d" nn k) (str cmd agent)))]
        (advance! branch c)
        (state! session_id (assoc st :k k))))))

(defn on-stop [in] (close! (get in "session_id") (reply-of in)))

(defn handle! [event input]
  (ensure-repo!)
  (with-lock
    #(case event
       "prompt" (on-prompt input)
       "tool" (on-tool input)
       "stop" (on-stop input)
       nil)))

(defn -main [& [event]]
  (let [input (try (json/parse-string (slurp *in*)) (catch Exception _ {}))]
    (try
      (handle! event input)
      (catch Throwable e
        (try (spit (gitdir "trace-errors.log")
                   (str (java.time.Instant/now) " " event " " (get input "session_id") " " (ex-message e) "\n")
                   :append true)
             (catch Throwable _))))
    (System/exit 0)))
