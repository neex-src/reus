(ns trace.shadow
  "The disk of a past session, rebuilt for a replay (trace.replay) without a checkout:
  trace.record asks what files exist, which project a cwd belongs to, what HEAD holds, and
  today's disk is not the disk of then. A shadow knows, per repo of a project and per
  worktree of it, which commit was checked out, and an overlay {path -> content | :deleted}
  of what the transcript proved on top of that commit. Files come from the project's real
  repo (read only: reflogs, objects through long-lived `git cat-file` processes), nothing is
  written to disk; paths are the transcript's own spelling all the way.

  (disk sh repo)  the trace.disk/Disk of the shadow; repo is the trace repo being written
                  (never traced), cwds outside every repo fall back to today's disk
  (close! sh)     end the cat-file processes

  Paths. The orig root of a cwd is found from its nearest existing ancestor, keeping the
  transcript's spelling (~/projects/tellij stays tellij although it is a symlink to
  deliberator now); :maps {orig-root repo-dir} covers repos that moved away. A root is
  named as trace.record names a project: its directory name (-1, -2 on a clash).

  HEAD. (sync! sh orig-path ts branch) puts the worktree that holds orig-path on the
  commit it had at ts (epoch ms), best source first:
    :reflog  that worktree's HEAD reflog (main checkout, worktrees that still exist)
    :branch  the reflog of the worktree's branch (from its `worktree add`), else of
             `branch` (the transcript entry's gitBranch, which is the session's, not
             necessarily the cwd's)
    :history `branch`'s first-parent history (or, deleted, the 2nd parent of its
             `Merge <branch>` commit), the last commit made before ts
    :main    the default branch's first-parent history before ts
  A move keeps the overlay as `git checkout --detach` keeps uncommitted changes; where git
  would refuse (a changed file differs between the two commits) it is `reset --hard`: the
  overlay of tracked files goes, files git does not track stay.

  Evidence. (write! sh orig-path content) / (delete! ...) put what the transcript proves
  into the overlay (trace.replay applies edits, bash edit diffs, full reads); a path
  outside every repo goes to an overlay of its own, which is read before today's disk."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [trace.disk :as disk]
            [trace.store :as store])
  (:import [java.io BufferedInputStream BufferedOutputStream ByteArrayOutputStream InputStream OutputStream]))

(defn- sh! [dir & args]
  (let [r (apply p/shell {:out :string :err :string :continue true :dir (str dir)} "git" args)]
    (when (zero? (:exit r)) (str/trim-newline (:out r)))))

(defn- under? [root p] (let [p (str p)] (or (= p root) (str/starts-with? p (str root "/")))))
(defn- swap-prefix [p from to] (str to (subs (str p) (count from))))
(def ^:private norms (java.util.concurrent.ConcurrentHashMap.))
(defn- norm
  "p without . and .. (remembered: a replay asks about the same few thousand paths again and again)."
  [p]
  (let [p (str p)]
    (or (.get norms p)
        (let [n (str (fs/normalize p))]
          (when (> (.size norms) 100000) (.clear norms))
          (.put norms p n)
          n))))

(defn create
  "A shadow set. maps: {orig-root repo-dir}."
  [maps]
  (atom {:maps (into {} (map (fn [[o d]] [(str o) (str (fs/canonicalize d))])) maps)
         :roots {}      ; orig-root -> {:repo :common :project :known #{orig-wt} :branches {orig-wt branch}}
         :wts {}        ; orig-wt (a root too) -> {:root :head sha :overlay {orig-path content|:deleted}}
         :abs {}        ; orig-path outside every repo -> content
         :cats {}       ; repo -> the cat-file processes
         :cache {}
         :stats {}}))

(defn- stat! [sh k] (swap! sh update-in [:stats k] (fnil inc 0)))

(defn stats [sh] (:stats @sh))

;; --- git objects of a repo ----------------------------------------------------------

(defn- spawn [& args]
  (let [pr (.start (doto (ProcessBuilder. ^java.util.List (vec args))
                     (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
    {:proc pr :in (BufferedOutputStream. (.getOutputStream pr)) :out (BufferedInputStream. (.getInputStream pr))}))

(defn- read-line* [^InputStream in]
  (let [bo (ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
        (cond (neg? b) nil
              (= b 10) (String. (.toByteArray bo) "UTF-8")
              :else (do (.write bo b) (recur)))))))

(defn- ask! [{:keys [^OutputStream in]} line]
  (.write in (.getBytes (str line "\n") "UTF-8"))
  (.flush in))

(defn- cat-of [sh repo]
  (or (get-in @sh [:cats repo])
      (let [c {:batch (spawn "git" "-C" repo "cat-file" "--batch")}]
        (swap! sh assoc-in [:cats repo] c)
        c)))

(defn close! "Stop the helper processes." [sh]
  (doseq [[_ {{:keys [proc]} :batch}] (:cats @sh)]
    (.destroy ^Process proc))
  (swap! sh assoc :cats {}))

(defn- tree-of
  "{path blob-sha} of a commit (ls-tree, kept for the most recent few commits)."
  [sh repo commit]
  (let [k [repo commit]]
    (or (get-in @sh [:cache :trees k])
        (let [out (:out (p/shell {:out :string :err :string :continue true :dir repo} "git" "ls-tree" "-r" "-z" commit))
              tree (into {} (keep #(when-let [[_ sha path] (re-matches #"(?s)\d+ blob ([0-9a-f]+)\t(.*)" %)]
                                     [(.intern ^String path) sha]))
                         (str/split out #"\u0000"))]
          (swap! sh update-in [:cache :trees]
                 (fn [m] (let [m (if (>= (count m) 12) (dissoc m (first (:cache-order @sh))) m)]
                           (assoc m k tree))))
          (swap! sh update :cache-order #(vec (take-last 11 (conj (or % []) k))))
          tree))))

(defn- blob-at
  "The sha of the blob at rel in commit, or nil."
  [sh repo commit rel]
  (get (tree-of sh repo commit) rel))

(defn- blob-bytes
  "The content of blob sha in repo."
  [sh repo sha]
  (or (get-in @sh [:cache :blobs sha])
      (let [c (:batch (cat-of sh repo))]
        (ask! c sha)
        (let [[_ _ size] (str/split (str (read-line* (:out c))) #" ")
              bs (.readNBytes ^InputStream (:out c) (parse-long size))]
          (.read ^InputStream (:out c))
          (when (> (count (get-in @sh [:cache :blobs])) 400) (swap! sh assoc-in [:cache :blobs] {}))
          (swap! sh assoc-in [:cache :blobs sha] bs)
          bs))))

;; --- where a path belongs ---------------------------------------------------------

(defn- git-dirs
  "[common-dir toplevel] of an existing dir, or nil."
  [dir]
  (some-> (sh! dir "rev-parse" "--path-format=absolute" "--git-common-dir" "--show-toplevel")
          str/split-lines))

(defn- origin
  "{:orig-root :repo :common} for an orig path: the repo it is in today, with the orig
  spelling of that repo's main checkout. nil when no repo leads there."
  [sh p]
  (let [p (str p)]
    (if-let [[o d] (first (sort-by (comp - count key) (filter #(under? (key %) p) (:maps @sh))))]
      (when-let [[common _] (git-dirs d)] {:orig-root o :repo d :common common})
      (when-let [anc (first (filter fs/directory? (take-while some? (iterate #(some-> % fs/parent str) p))))]
        (when-let [[common _] (git-dirs anc)]
          (let [main (str (fs/parent common))
                canon (str (fs/canonicalize anc))]
            (when (= ".git" (str (fs/file-name common)))
              {:orig-root (if (under? main canon)
                            (let [rel (subs canon (count main))] (subs anc 0 (- (count anc) (count rel))))
                            main)
               :repo main :common common})))))))

(defn- deepest [p ks]
  (let [p (str p)]
    (reduce (fn [best k] (if (and (or (nil? best) (> (count k) (count best))) (under? k p)) k best)) nil ks)))

(defn- root-of [sh p] (deepest p (keys (:roots @sh))))

(defn- open-root!
  "The root of the repo orig path p is in, opened on first use."
  [sh p]
  (or (root-of sh p)
      (when-let [{:keys [orig-root repo common]} (origin sh p)]
        (let [clean (disk/project-name (fs/file-name orig-root))
              taken (set (map :project (vals (:roots @sh))))
              project (first (remove taken (cons clean (map #(str clean "-" %) (iterate inc 1)))))
              live-wts (->> (str/split-lines (or (sh! repo "worktree" "list" "--porcelain") ""))
                            (keep #(second (re-find #"^worktree (.+)$" %)))
                            (remove #{repo}))]
          (swap! sh (fn [s]
                      (-> s
                          (assoc-in [:roots orig-root]
                                    {:repo repo :common common :project project :branches {}
                                     :known (into #{} (keep #(when (under? repo %) (swap-prefix % repo orig-root))) live-wts)})
                          (assoc-in [:wts orig-root] {:root orig-root :head nil :overlay {}})
                          (update :cache dissoc :owner))))
          orig-root))))

(defn worktree!
  "Note that orig dir wt was a git worktree (a `git worktree add` in a transcript), on
  branch (nil when unknown or detached)."
  [sh wt & [branch]]
  (when-let [r (open-root! sh wt)]
    (when (not= r (str wt)) (swap! sh update-in [:roots r :known] conj (str wt)))
    (when branch (swap! sh assoc-in [:roots r :branches (str wt)] branch))))


(defn- wt-of
  "[orig-root orig-worktree] holding orig path p (the worktree is the root itself when
  p is in no known worktree)."
  [sh p]
  (when-let [r (open-root! sh p)]
    [r (or (deepest p (get-in @sh [:roots r :known])) r)]))

(defn- owner*
  [sh p]
  (let [s @sh]
    (when-let [r (root-of sh p)]
      [r (reduce (fn [best [wt v]]
                   (if (and (= r (:root v)) (or (nil? best) (> (count wt) (count best))) (under? wt p)) wt best))
                 nil (:wts s))])))

(defn- owner
  "[root created-worktree] of an orig path in an opened root, else nil. Remembered until a
  root or worktree is added."
  [sh p]
  (let [m (get-in @sh [:cache :owner])]
    (if-let [[_ v] (find m p)]
      v
      (let [v (owner* sh (str p))]
        (swap! sh assoc-in [:cache :owner] (assoc (if (> (count m) 50000) {} m) p v))
        v))))

(defn- rel-in [wt p] (if (= p wt) "" (subs p (inc (count wt)))))

;; --- which commit was checked out -------------------------------------------------

(defn- reflog
  "[[epoch-ms sha]] of a reflog file, oldest first."
  [sh f]
  (or (get-in @sh [:cache :reflog f])
      (let [es (when (fs/exists? f)
                 (vec (keep #(when-let [[_ new t] (re-find #"^[0-9a-f]+ ([0-9a-f]+) .*?> (\d+) [-+]\d{4}\t" %)]
                               (when-not (re-find #"^0+$" new) [(* 1000 (parse-long t)) new]))
                            (str/split-lines (slurp f)))))]
        (swap! sh assoc-in [:cache :reflog f] es)
        es)))

(defn- at [entries ts] (last (take-while #(<= (first %) ts) entries)))

(defn- cached [sh k f]
  (let [c (:cache @sh)]
    (if (contains? c k) (get c k) (let [v (f)] (swap! sh assoc-in [:cache k] v) v))))

(defn- history
  "[[epoch-ms sha]] of rev's first-parent history, oldest first."
  [sh repo rev]
  (cached sh [:history repo rev]
          #(vec (reverse (keep (fn [l] (let [[sha t] (str/split l #" ")] (when t [(* 1000 (parse-long t)) sha])))
                               (str/split-lines (or (sh! repo "rev-list" "--first-parent" "--format=%H %ct" "--no-commit-header" rev) "")))))))

(defn- merged-tip
  "The tip a deleted branch had when it was merged: 2nd parent of its merge commit."
  [sh repo branch]
  (let [merges (cached sh [:merges repo]
                       #(vec (str/split-lines (or (sh! repo "log" "--all" "--merges" "--format=%H %P %s") ""))))]
    (cached sh [:merged repo branch]
            #(let [v (some (fn [l] (let [[_ p2 s] (str/split l #" " 3)]
                                     (when (and p2 (or (str/starts-with? (str s) (str "Merge " branch ":"))
                                                       (str/starts-with? (str s) (str "Merge " branch " "))
                                                       (= (str s) (str "Merge " branch))
                                                       (str/starts-with? (str s) (str "Merge branch '" branch "'"))))
                                       p2)))
                           merges)]
               (when v (second (str/split v #" ")))))))

(defn- admin-dir
  "The .git/worktrees/<n> dir of the worktree at real path wt-repo."
  [sh common wt-repo]
  (cached sh [:admin common wt-repo]
          #(some (fn [d] (let [g (fs/path d "gitdir")]
                           (when (and (fs/exists? g) (= (str/trim (slurp (str g))) (str wt-repo "/.git"))) d)))
                 (when (fs/exists? (fs/path common "worktrees")) (fs/list-dir (fs/path common "worktrees"))))))

(defn- branch-ref? [sh repo b]
  (cached sh [:branch-ref repo b] #(boolean (sh! repo "rev-parse" "-q" "--verify" (str "refs/heads/" b)))))

(defn- head-at
  "[sha source] of orig worktree wt (in root r) at ts."
  [sh r wt ts branch]
  (let [{:keys [repo common]} (get-in @sh [:roots r])
        wt-repo (if (= r wt) repo (swap-prefix wt r repo))
        admin (when-not (= r wt) (admin-dir sh common wt-repo))
        head-log (if (= r wt) (str (fs/path common "logs" "HEAD")) (some-> admin (fs/path "logs" "HEAD") str))
        branch (when-not (contains? #{nil "" "HEAD"} branch) branch)
        default (cached sh [:default repo] #(some (fn [b] (when (branch-ref? sh repo b) b)) ["main" "master"]))]
    (or (some-> head-log (#(at (reflog sh %) ts)) second (vector :reflog))
        (some-> branch (#(at (reflog sh (str (fs/path common "logs" "refs" "heads" %))) ts)) second (vector :branch))
        (some-> branch (#(or (when (branch-ref? sh repo %) (str "refs/heads/" %))
                             (merged-tip sh repo %)))
                (#(at (history sh repo %) ts)) second (vector :history))
        (some-> default (#(at (history sh repo %) ts)) second (vector :main)))))

;; --- files ----------------------------------------------------------------------------

(defn- lookup
  "What the file at orig path p is: {:value content} (the overlay), {:blob sha :repo repo}
  (the commit checked out), {:real p} (today's disk, outside every repo), or nil."
  [sh p]
  (let [p (norm p)]
    (if-let [[r wt] (owner sh p)]
      (let [{:keys [head overlay]} (get-in @sh [:wts wt])]
        (cond
          (contains? overlay p)
          (let [v (get overlay p)] (when-not (= :deleted v) {:value v}))

          ;; a linked worktree has a .git file naming its admin dir
          (and (not= r wt) (= p (str wt "/.git")))
          (let [{:keys [repo common]} (get-in @sh [:roots r])]
            {:value (str "gitdir: " (or (admin-dir sh common (swap-prefix wt r repo))
                                        (str common "/worktrees/" (fs/file-name wt))) "\n")})

          :else
          (when head
            (when-let [b (blob-at sh (get-in @sh [:roots r :repo]) head (rel-in wt p))]
              {:blob b :repo (get-in @sh [:roots r :repo])}))))
      (if-let [v (get-in @sh [:abs p])]
        {:value v}
        (when (try (fs/regular-file? p) (catch Exception _ false)) {:real p})))))

(defn- file-bytes [sh p]
  (when-let [{:keys [value blob repo real]} (lookup sh p)]
    (cond value (store/bytes-of value)
          blob (blob-bytes sh repo blob)
          real (fs/read-all-bytes real))))

(defn content
  "The file at orig path p as a string, or nil."
  [sh p]
  (some-> (file-bytes sh p) (String. "UTF-8")))

(defn- move!
  "Put worktree wt on commit new, keeping the overlay as git checkout would."
  [sh wt new]
  (let [{:keys [root head overlay]} (get-in @sh [:wts wt])
        repo (get-in @sh [:roots root :repo])
        at* (fn [commit p] (when commit (blob-at sh repo commit (rel-in wt p))))
        modified? (fn [p v] (let [o (at* head p)] (if (= v :deleted) (some? o) (not= o (store/blob-sha v)))))
        conflict? (some (fn [[p v]] (and (modified? p v) (not= (at* head p) (at* new p)))) overlay)
        overlay (if conflict?
                  (into {} (remove (fn [[p _]] (or (at* head p) (at* new p)))) overlay)
                  overlay)]
    (swap! sh update-in [:wts wt] assoc :head new :overlay overlay)))

(defn- create-wt!
  "Worktree wt of root r is first checked out, on sha; what the transcript wrote under it
  before is in it."
  [sh r wt sha]
  (swap! sh (fn [s]
              (let [up (deepest wt (remove #{wt} (keep #(when (= r (:root (val %))) (key %)) (:wts s))))
                    mine (filter #(under? wt (key %)) (get-in s [:wts up :overlay]))]
                (-> s
                    (update-in [:wts up :overlay] #(apply dissoc % (map key mine)))
                    (assoc-in [:wts wt] {:root r :head sha :overlay (into {} mine)})
                    (update :cache dissoc :owner))))))

(defn sync!
  "Make the worktree holding orig path p be on its commit at ts (epoch ms). Returns the
  HEAD source, or nil when p is in no repo."
  [sh p ts branch & _]
  (when-let [[r wt] (wt-of sh (norm p))]
    (let [[sha src] (head-at sh r wt ts (or (get-in @sh [:roots r :branches wt]) branch))]
      (when sha
        (cond
          (not (get-in @sh [:wts wt]))
          (do (when-not (get-in @sh [:wts r :head])
                (when-let [[s0] (head-at sh r r ts nil)] (move! sh r s0)))
              (create-wt! sh r wt sha))

          (not= sha (get-in @sh [:wts wt :head]))
          (move! sh wt sha))
        (stat! sh src))
      src)))

;; --- evidence ------------------------------------------------------------------------

(defn writable? "Can orig path p be written (an absolute path)?" [sh p] (boolean (and p (fs/absolute? p))))

(defn write! [sh p content]
  (when (writable? sh p)
    (let [p (norm p)]
      (if-let [[_ wt] (owner sh p)]
        (swap! sh assoc-in [:wts wt :overlay p] content)
        (swap! sh assoc-in [:abs p] content)))
    true))

(defn delete! [sh p]
  (when (writable? sh p)
    (let [p (norm p)]
      (if-let [[r wt] (owner sh p)]
        (let [{:keys [head]} (get-in @sh [:wts wt])
              tracked? (and head (blob-at sh (get-in @sh [:roots r :repo]) head (rel-in wt p)))]
          (swap! sh (fn [s] (if tracked? (assoc-in s [:wts wt :overlay p] :deleted) (update-in s [:wts wt :overlay] dissoc p)))))
        (swap! sh update :abs dissoc p)))))

;; --- the Disk -------------------------------------------------------------------------

(defn disk
  "The trace.disk/Disk of shadow sh; repo is the trace repo being written."
  [sh repo]
  (let [real (disk/live repo)]
    (reify disk/Disk
      (project-of [_ cwd]
        (if-let [[r wt] (and cwd (owner sh (norm cwd)))]
          {:project (get-in @sh [:roots r :project]) :root wt}
          (disk/project-of real cwd)))
      (resolve-file [_ tok dirs]
        (try
          (let [c (cond (str/blank? tok) nil
                        (fs/absolute? tok) (norm tok)
                        :else (some #(let [c (norm (fs/path % tok))] (when (lookup sh c) c)) dirs))
                found (when c (lookup sh c))]
            (when found (if-let [f (:real found)] (str (fs/canonicalize f)) c)))
          (catch Exception _ nil)))
      (file-bytes [_ f] (file-bytes sh f))
      (head-content [_ root rel]
        (if-let [{:keys [head] r :root} (get-in @sh [:wts (norm root)])]
          (let [repo (get-in @sh [:roots r :repo])]
            (if-let [b (and head (blob-at sh repo head rel))] (String. ^bytes (blob-bytes sh repo b) "UTF-8") ""))
          (disk/head-content real root rel)))
      (external-text [_ f] (disk/external-text real f)))))
