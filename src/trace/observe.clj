(ns trace.observe
  "What a hook event must take from the machine at the moment it fires, before Claude goes on: the
  worktree the event acted in, hashed into the trace repo (its tree is the snapshot, exact because
  nothing has run since), that worktree's HEAD, the files Claude Code may delete later (a persisted
  tool output, a background task's output, the CLAUDE.md chain and memory files of a prompt) and the
  transcript's size. Everything else (commits, refs, notes, the mirror, what the transcript says) is
  made from this observation later, by trace.record.

  (observe d event input opts) -> obs, never throws:
    {:at ms
     :wt {:project :root :common :git-dir :snap :head}   the worktree the event acted in (:snap, its
                                                       trace.store tree spec, when it was hashed;
                                                       none outside git)
     :snap-error msg  :slow? bool                       hashing failed / took too long (no :snap)
     :cwd-project {:project :root ...}                  the project of the payload cwd (prompt, subagent-start)
     :file path                                         the one file a tool call is about
     :output text                                       a tool's full output when Claude Code kept it
                                                       in a file; a prompt's <output-file>
     :files {path content}                              prompt / subagent-start: memory + CLAUDE.md
     :tsize n}                                          bytes of the transcript at that moment
  opts: :last-root (the worktree this session or agent last acted in, from the previous events),
        :name-of (fn [project-map] name): the project's name in the trace repo (same-named checkouts
        of different repos get different names).

  Which worktree: a tool call's own file path, a leading `cd X &&`, `git -C X`, or the shell's cwd
  for a shell call (call-dir); nothing for a call that does not act in one (an Agent, a web fetch):
  it keeps the previous tree. A prompt: the worktree its session last acted in when that is of the
  cwd's project (no spurious switch between worktrees), else the cwd's. A subagent start: none (its
  first commit is cut from its parent's tip, with that tree)."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [trace.disk :as disk]))

(defn text-of [r]
  (cond
    (nil? r) ""
    (string? r) r
    (sequential? r) (str/join "\n" (map text-of r))
    (map? r) (let [parts (keep #(let [v (get r %)] (when-not (nil? v) (text-of v)))
                               ["stdout" "stderr" "text" "content" "output" "result" "file" "error"])]
               (if (seq parts) (str/join "\n" (remove str/blank? parts)) (json/generate-string r)))
    :else (str r)))

(defn call-dir
  "Where a call demonstrably acted: the directory of the file it names (the dir itself when it names
  one), of a leading `cd X &&` or a `git -C X`, or the shell's cwd for a shell call. Nil for any other
  call (an Agent, a web fetch, an MCP tool without a path ...): it says nothing about a worktree."
  [{:strs [tool_input cwd]}]
  (let [abs (fn [d] (let [d (str/replace (str d) #"^['\"]|['\"]$" "")
                          d (if (str/starts-with? d "~") (str (fs/home) (subs d 1)) d)]
                      (cond (str/blank? d) nil
                            (str/starts-with? d "/") d
                            cwd (str (fs/path cwd d)))))]
    (or (when-let [f (some #(get tool_input %) ["file_path" "path" "notebook_path"])]
          (when (str/starts-with? (str f) "/")
            (if (fs/directory? f) (str f) (str (fs/parent f)))))
        (when-let [cmd (get tool_input "command")]
          (or (some-> (or (re-find #"^\s*cd\s+(\S+)\s*(?:&&|;)" cmd) (re-find #"git\s+-C\s+(\S+)" cmd)) second abs fs/normalize str)
              cwd)))))

(def read-tools #{"Read" "NotebookRead" "read_file" "Glob" "Grep" "LS" "WebFetch" "WebSearch"})

(def ^:private read-commands
  #{"ls" "cat" "head" "tail" "less" "more" "grep" "rg" "egrep" "fgrep" "find" "fd" "wc" "pwd" "echo"
    "printf" "stat" "file" "tree" "du" "df" "which" "type" "env" "printenv" "date" "cd" "true" "sort"
    "uniq" "cut" "diff" "jq" "readlink" "realpath" "basename" "dirname" "id" "whoami" "uname" "uptime"
    "nproc" "column" "tr" "nl" "comm" "cmp" "md5sum" "sha1sum" "sha256sum" "xxd" "od" "hexdump"})

(def ^:private read-git
  #{"log" "show" "status" "diff" "rev-parse" "ls-files" "ls-tree" "cat-file" "blame" "grep" "describe"
    "shortlog" "reflog" "rev-list" "for-each-ref" "show-ref" "merge-base" "count-objects" "help" "version"})

(defn- read-segment? [seg]
  (let [ws (remove #(re-matches #"[A-Za-z_][A-Za-z0-9_]*=.*" %) (str/split (str/trim seg) #"\s+"))
        [w & more] ws]
    (cond (str/blank? w) true
          (= "git" (fs/file-name w))
          (let [sub (loop [[a b & r :as xs] more]
                      (cond (empty? xs) nil
                            (#{"-C" "-c" "--git-dir" "--work-tree"} a) (recur r)
                            (str/starts-with? (str a) "-") (recur (rest xs))
                            :else a))]
            (boolean (read-git sub)))
          :else (contains? read-commands (str (fs/file-name w))))))

(defn read-only?
  "Does this call certainly change nothing on disk? The reading tools, and a shell command whose every
  part is a known reader, with no redirect into a file. Anything else counts as a write."
  [{:strs [tool_name tool_input]}]
  (let [cmd (get tool_input "command")]
    (cond (read-tools (str/replace (str tool_name) #"^mcp__.*__" "")) true
          (string? cmd) (let [c (str/replace cmd #"[0-9]?>&[0-9]" "")] ; 2>&1 writes no file
                          (and (not (re-find #">(?!\s*/dev/null)" c))
                               (every? read-segment? (str/split c #"\|\||&&|[;|&\n]"))))
          :else false)))

(defn worktree-of
  "The project a directory is in, walking up from a dir that is not there (yet, or any more)."
  [d dir]
  (loop [x dir n 0]
    (or (disk/project-of d x)
        (when (and (< n 6) (fs/parent x)) (recur (str (fs/parent x)) (inc n))))))

(defn the-file
  "The one existing regular file a call is about: its file_path/path/notebook_path, else the one
  existing file of the root a shell command names. Canonical, as the session knew it."
  [d root {:strs [tool_input cwd]}]
  (let [resolve (fn [tok]
                  (disk/resolve-file d (str/replace (str tok) #"^['\"]|['\"]$" "") [(or cwd root) root]))]
    (if-let [fp (some #(get tool_input %) ["file_path" "path" "notebook_path"])]
      (resolve fp)
      (when-let [cmd (get tool_input "command")]
        (when root
          (let [fs* (distinct (filter #(str/starts-with? % (str root "/"))
                                      (keep resolve (str/split cmd #"[\s;|&<>()]+"))))]
            (when (= 1 (count fs*)) (first fs*))))))))

(defn- persisted-output
  "A tool's full output when Claude Code kept it in a file and the result only previews it."
  [d in]
  (let [text (text-of (get in "tool_response"))
        p (or (get-in in ["tool_response" "persistedOutputPath"])
              (second (re-find #"Full output saved to: (\S+\.txt)" text)))]
    (some->> p (disk/external-text d))))

(defn- tsize [f] (try (when (and f (fs/regular-file? f)) (fs/size f)) (catch Exception _ nil)))

(defn- hashed
  "p with the tree of its worktree and its HEAD; a failed hash leaves :snap out and says why."
  [d p]
  (when p
    (let [head (try (disk/real-head d p) (catch Exception _ nil))]
      (try
        (let [t (disk/snapshot d p)]
          {:wt (cond-> (assoc p :head head) t (assoc :snap t))})
        (catch Exception e
          {:wt (assoc p :head head) :snap-error (or (ex-message e) (str e)) :slow? (boolean (:slow (ex-data e)))})))))

(defn- sha1-hex [s]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md (.getBytes (str s) "UTF-8"))))))

(defn project-namer
  "A name-of over a name -> repo kv, (lookup name) and (claim! name id): a project is named after its
  checkout's directory, unless another repo (another git common dir, or root outside git) has that
  name already; then <name>-<6 hex of its own id>."
  [lookup claim!]
  (fn [{:keys [project common root]}]
    (let [id (str (or common root))
          own (fn [n] (let [cur (lookup n)] (cond (nil? cur) (do (claim! n id) n) (= cur id) n)))]
      (or (own project)
          (own (str project "-" (subs (sha1-hex id) 0 6)))
          project))))

(defn observe
  "See ns."
  [d event input {:keys [last-root name-of] :or {name-of :project}}]
  (let [{:strs [cwd session_id transcript_path prompt]} input
        named (fn [p] (when p (assoc p :project (name-of p))))
        guard (fn [f] (try (f) (catch Exception e {:error (or (ex-message e) (str e))})))]
    (merge
     {:at (System/currentTimeMillis) :tsize (tsize transcript_path)}
     (guard
      #(case event
         "tool"
         (let [dir (call-dir input)
               ;; a dir that is not there (a worktree removed under the session): the repo above it only
               ;; when the session was working in that repo, never a stranger the dir happens to sit in
               p (when dir
                   (if (fs/directory? dir)
                     (disk/project-of d dir)
                     (let [up (worktree-of d dir) was (some->> last-root (disk/project-of d))]
                       (when (and up was (= (:common up) (:common was))) up))))
               p (named p)
               read? (read-only? input)
               ;; a read keeps the worktree the session (agent) last wrote in, or its cwd's: looking at main
               ;; from a worktree, or at another repo, moves nothing (only a write does)
               home (when (and p read?)
                      (named (or (some->> last-root (disk/project-of d)) (disk/project-of d cwd))))]
           (merge (hashed d (or home p))
                  {:read? read?
                   :file (the-file d (or (:root p) cwd) input) :output (persisted-output d input)}))

         "prompt"
         (let [cwdp (named (disk/project-of d cwd))
               lastp (when last-root (named (disk/project-of d last-root)))
               p (if (and lastp (or (nil? cwdp) (= (:project lastp) (:project cwdp)))) lastp cwdp)]
           (merge (hashed d p)
                  {:cwd-project cwdp
                   :files (disk/trace-files d session_id cwd transcript_path)
                   :output (some->> (re-find #"<output-file>(.+?)</output-file>" (str prompt)) second str/trim
                                    (disk/external-text d))}))

         "subagent-start" ; no hash: the agent's first commit keeps the tree of its parent's tip
         {:cwd-project (named (disk/project-of d cwd))
          :files (disk/trace-files d session_id cwd nil)}

         {})))))
