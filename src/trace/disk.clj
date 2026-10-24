(ns trace.disk
  "What trace.record reads from the machine around a session. Paths are the session's own
  spelling. (live repo) is the real filesystem and git; a replay has its own
  (trace.shadow/disk): the disk as it was then.

  (project-of d cwd)         {:project name :root worktree-dir :common git-common-dir :git-dir} or
                             nil when the cwd is not traced (:common nil outside git)
  (resolve-file d tok dirs)  the canonical path of the existing regular file a token names
                             (absolute, or relative to the first of dirs that has it), or nil
  (file-bytes d f)           its bytes, or nil
  (head-content d root rel)  rel as the checkout at root has it committed, \"\" when it is not
  (external-text d f)        the text of a file Claude Code keeps outside the project (a persisted
                             tool output, a background task's output), or nil when it is gone
  (snapshot d p)             the tree spec {:base sha} of the whole worktree of project p
                             ({:root :common :git-dir}) as it is now: tracked and untracked
                             files that are not ignored, as `git add -A` takes them;
                             nil when the worktree is gone
  (real-head d p)            the commit the worktree has checked out, or nil
  (main-tip d p)             the project's main (main, else master) tip, or nil
  (commit-info d p sha)      {:tree :parents :author :committer :message} of a real commit
                             (idents as \"Name <mail> epoch +zone\")
  (chain d p from to)        the first-parent commits after from up to to, oldest first (just to
                             when from is nil)
  (range-set d p excl incl)  the commits that are in incl and not in excl
  (trace-files d session cwd transcript)
                             {\"memory/<f>\" content \"claude-md/<f>\" content}: the project's
                             memory files and the CLAUDE.md chain in effect"
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defprotocol Disk
  (project-of [d cwd])
  (resolve-file [d tok dirs])
  (file-bytes [d f])
  (head-content [d root rel])
  (external-text [d f])
  (snapshot [d p])
  (real-head [d p])
  (main-tip [d p])
  (commit-info [d p sha])
  (chain [d p from to])
  (range-set [d p excl incl])
  (trace-files [d session cwd transcript]))

(defn project-name [s] (str/replace (str s) #"[^A-Za-z0-9._-]" "_"))

(defn projects-root
  "Where the projects are, for a cwd outside git: $TRACE_PROJECTS, else ~/projects."
  []
  (or (System/getenv "TRACE_PROJECTS") (str (fs/path (fs/home) "projects"))))

(defn projects-project
  "The project of a cwd outside git: the <projects-root>/<name> it sits in."
  [cwd]
  (let [projects (projects-root)]
    (when-let [[_ n] (re-find (re-pattern (str "^\\Q" projects "\\E/([^/]+)")) cwd)]
      {:project (project-name n) :root (str (fs/path projects n))})))

(defn- shell-git
  "git in dir; a dir that is not there (a removed worktree) is a failed run, never an exception."
  [dir & args]
  (if (and dir (fs/directory? (str dir)))
    (apply p/shell {:out :string :err :string :continue true :dir (str dir)} "git" args)
    {:exit 128 :out "" :err (str "no such directory: " dir)}))

(defn- out [r] (when (zero? (:exit r)) (str/trim (:out r))))

(defn git-in
  "Trimmed stdout of git run in dir, or nil when it fails."
  [dir & args] (out (apply shell-git dir args)))

(defn default-branch
  "main, else master: the one that exists in the repo at dir."
  [dir]
  (some #(when (git-in dir "rev-parse" "--verify" "-q" (str "refs/heads/" %)) %) ["main" "master"]))

(defn parse-info
  "The map commit-info returns, from `git log -1 --format=INFO-FORMAT` output."
  [s]
  (let [[an ae ad cn ce cd ps t msg] (str/split s #"\u0001" 9)]
    {:author (str an " <" ae "> " ad) :committer (str cn " <" ce "> " cd)
     :parents (vec (remove str/blank? (str/split ps #" "))) :tree t :message (str/trimr (str msg))}))

(def info-format "--format=%an%x01%ae%x01%ad%x01%cn%x01%ce%x01%cd%x01%P%x01%T%x01%B")

(defn real-info [dir sha]
  (some-> (git-in dir "log" "-1" "--date=raw" info-format sha) parse-info))

(defn real-chain [dir from to]
  (if from
    (vec (str/split-lines (or (git-in dir "rev-list" "--first-parent" "--reverse" (str from ".." to)) "")))
    [to]))

(defn real-range [dir excl incl]
  (set (remove str/blank? (str/split-lines (or (git-in dir "rev-list" (str excl ".." incl)) "")))))

(defn claude-md-key
  "claude-md/<path with ~ for home and __ for />."
  [path]
  (let [home (str (fs/home))
        p (if (str/starts-with? path (str home "/")) (str "~" (subs path (count home))) path)]
    (str "claude-md/" (str/replace (str/replace p #"^/" "") "/" "__"))))

(defn memory-key [path] (str "memory/" (fs/file-name path)))

(defn live-trace-files
  "Memory files of the session's project (next to its transcript) and the CLAUDE.md chain from disk."
  [cwd transcript]
  (let [read #(try (slurp (str %)) (catch Exception _ nil))
        md (concat [(fs/path (fs/home) ".claude" "CLAUDE.md")]
                   (when cwd
                     (mapcat (fn [d] (map #(fs/path d %) ["CLAUDE.md" ".claude/CLAUDE.md" "CLAUDE.local.md"]))
                             (reverse (take-while some? (iterate #(some-> % fs/parent) (fs/path cwd)))))))
        mem (when transcript (fs/path (fs/parent transcript) "memory"))]
    (into {}
          (concat (for [f md :when (fs/regular-file? f) :let [c (read f)] :when c] [(claude-md-key (str f)) c])
                  (when (and mem (fs/directory? mem))
                    (for [f (fs/glob mem "**.md") :when (fs/regular-file? f) :let [c (read f)] :when c]
                      [(str "memory/" (str (fs/relativize mem f))) c]))))))

(defrecord Live [repo]
  Disk
  (project-of [_ cwd]
    (when (and cwd (fs/directory? cwd))
      (let [disk (str (fs/canonicalize cwd))
            r (shell-git disk "rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel" "--absolute-git-dir")]
        (cond
          (str/starts-with? (str disk "/") (str (fs/canonicalize (fs/create-dirs repo)) "/")) nil
          (zero? (:exit r))
          (let [[common top gitdir] (str/split-lines (str/trim (:out r)))
                home (if (= ".git" (str (fs/file-name common))) (fs/parent common) top)]
            {:project (project-name (fs/file-name home)) :root top :common common :git-dir gitdir})
          :else (projects-project disk)))))
  (resolve-file [_ tok dirs]
    (let [f (cond (str/blank? tok) nil
                  (fs/absolute? tok) tok
                  :else (some #(let [c (str (fs/path % tok))] (when (fs/exists? c) c)) dirs))]
      (when (and f (fs/regular-file? f)) (str (fs/canonicalize f)))))
  (file-bytes [_ f] (when (fs/regular-file? f) (fs/read-all-bytes f)))
  (head-content [_ root rel]
    (let [r (shell-git root "show" (str "HEAD:" rel))]
      (if (zero? (:exit r)) (:out r) "")))
  (external-text [_ f] (when (and f (fs/regular-file? f)) (slurp (str f))))
  (snapshot [_ {:keys [root common git-dir]}]
    ;; a private index per worktree, seeded from the worktree's own so the first look is cheap;
    ;; objects are written to the trace repo and read from the project's too
    (when (fs/directory? (str root))
      (let [idx (str (fs/path repo ".git" "trace-index" (project-name root)))
            objects (str (fs/path repo ".git" "objects"))
            env (if common
                  {"GIT_INDEX_FILE" idx "GIT_OBJECT_DIRECTORY" objects
                   "GIT_ALTERNATE_OBJECT_DIRECTORIES" (str (fs/path common "objects"))}
                  {"GIT_INDEX_FILE" idx "GIT_DIR" (str (fs/path repo ".git")) "GIT_WORK_TREE" (str root)})
            sh (fn [& args] (apply p/shell {:out :string :err :string :continue true :dir (str root) :extra-env env} "git" args))]
        (when-not (fs/exists? idx)
          (fs/create-dirs (fs/parent idx))
          (when (and git-dir (fs/exists? (fs/path git-dir "index"))) (fs/copy (fs/path git-dir "index") idx)))
        (sh "add" "-A" "--ignore-errors")
        (when-let [t (out (sh "write-tree"))] {:base t}))))
  (real-head [_ {:keys [root]}] (git-in root "rev-parse" "HEAD"))
  (main-tip [_ {:keys [root common]}]
    (when common
      (or (git-in root "rev-parse" "--verify" "-q" "refs/heads/main")
          (git-in root "rev-parse" "--verify" "-q" "refs/heads/master"))))
  (commit-info [_ {:keys [root]} sha] (real-info root sha))
  (chain [_ {:keys [root]} from to] (real-chain root from to))
  (range-set [_ {:keys [root]} excl incl] (real-range root excl incl))
  (trace-files [_ _ cwd transcript] (live-trace-files cwd transcript)))

(defn live
  "The real disk; repo is the trace repo, whose own directory is never traced."
  [repo] (->Live (str repo)))
