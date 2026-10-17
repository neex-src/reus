(ns trace.disk
  "What trace.record reads from the machine around a session: which project a cwd belongs
  to, which existing file a path or a shell token names, a file's bytes, a file at HEAD.
  Paths are the session's own spelling. (live repo) is the real filesystem and git; a replay
  has its own (trace.shadow/disk): the disk as it was then.

  (project-of d cwd)           {:project name :root dir} or nil when the cwd is not traced
  (resolve-file d tok dirs)    the canonical path of the existing regular file a token names
                               (absolute, or relative to the first of dirs that has it), or nil
  (file-bytes d f)             its bytes, or nil
  (head-content d root rel)    rel as the checkout at root has it committed, \"\" when it is not"
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defprotocol Disk
  (project-of [d cwd])
  (resolve-file [d tok dirs])
  (file-bytes [d f])
  (head-content [d root rel]))

(defn project-name [s] (str/replace (str s) #"[^A-Za-z0-9._-]" "_"))

(defn projects-project
  "The project of a cwd outside git: the ~/projects/<name> it sits in."
  [cwd]
  (let [projects (str (fs/path (fs/home) "projects"))]
    (when-let [[_ n] (re-find (re-pattern (str "^\\Q" projects "\\E/([^/]+)")) cwd)]
      {:project (project-name n) :root (str (fs/path projects n))})))

(defn- shell-git [dir & args]
  (apply p/shell {:out :string :err :string :continue true :dir (str dir)} "git" args))

(defrecord Live [repo]
  Disk
  (project-of [_ cwd]
    (when (and cwd (fs/directory? cwd))
      (let [disk (str (fs/canonicalize cwd))
            r (shell-git disk "rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel")]
        (cond
          (str/starts-with? (str disk "/") (str (fs/canonicalize (fs/create-dirs repo)) "/")) nil
          (zero? (:exit r))
          (let [[common top] (str/split-lines (str/trim (:out r)))
                home (if (= ".git" (str (fs/file-name common))) (fs/parent common) top)]
            {:project (project-name (fs/file-name home)) :root top})
          :else (projects-project disk)))))
  (resolve-file [_ tok dirs]
    (let [f (cond (str/blank? tok) nil
                  (fs/absolute? tok) tok
                  :else (some #(let [c (str (fs/path % tok))] (when (fs/exists? c) c)) dirs))]
      (when (and f (fs/regular-file? f)) (str (fs/canonicalize f)))))
  (file-bytes [_ f] (when (fs/regular-file? f) (fs/read-all-bytes f)))
  (head-content [_ root rel]
    (let [r (shell-git root "show" (str "HEAD:" rel))]
      (if (zero? (:exit r)) (:out r) ""))))

(defn live
  "The real disk; repo is the trace repo, whose own directory is never traced."
  [repo] (->Live (str repo)))
