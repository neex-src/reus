(ns trace.shadow
  "The disk of a past session, rebuilt for a replay (trace.replay): trace.record reads
  files, asks git where a cwd belongs and shows HEAD, and today's disk is not the disk
  of then. A shadow is a `git clone --shared --no-checkout` of a project's repo under
  <base>/<project>; a dir that was a worktree of it gets a shadow worktree at the same
  relative path. The real repos are only read (reflogs, objects), never written.

  Paths. A transcript names paths as the session knew them (orig); (local sh p) is
  where p is in the shadow, (orig sh p) the inverse; paths outside every shadow are
  left alone and are never written. The orig root of a cwd is found from its nearest
  existing ancestor, keeping the transcript's spelling (~/projects/tellij stays tellij
  although it is a symlink to deliberator now); :maps {orig-root repo-dir} covers
  repos that moved away.

  HEAD. (sync! sh orig-path ts branch) puts the worktree that holds orig-path on the
  commit it had at ts (epoch ms), best source first:
    :reflog  that worktree's HEAD reflog (main checkout, worktrees that still exist)
    :branch  the reflog of `branch` (the transcript entry's gitBranch)
    :history `branch`'s first-parent history (or, deleted, the 2nd parent of its
             `Merge <branch>` commit), the last commit made before ts
    :main    the default branch's first-parent history before ts
  A move is `git checkout --detach` (uncommitted changes carried along, as a real
  checkout or commit leaves them), `reset --hard` when that refuses.

  Evidence. (write! sh orig-path content) / (delete! ...) put what the transcript proves
  into the shadow (trace.replay applies edits, bash edit diffs, full reads)."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn- sh! [dir & args]
  (let [r (apply p/shell {:out :string :err :string :continue true :dir (str dir)} "git" args)]
    (when (zero? (:exit r)) (str/trim-newline (:out r)))))

(defn- under? [root p] (let [p (str p)] (or (= p root) (str/starts-with? p (str root "/")))))
(defn- swap-prefix [p from to] (str to (subs (str p) (count from))))

(defn create
  "A shadow set rooted at base (created). maps: {orig-root repo-dir}."
  [base maps]
  (fs/create-dirs base)
  (atom {:base (str (fs/canonicalize base))
         :maps (into {} (map (fn [[o d]] [(str o) (str (fs/canonicalize d))])) maps)
         :roots {}      ; orig-root -> {:shadow :repo :common :project :worktrees {orig-wt shadow-wt}}
         :heads {}      ; shadow worktree -> sha it is on
         :cache {}
         :stats {}}))

(defn- stat! [sh k] (swap! sh update-in [:stats k] (fnil inc 0)))

(defn stats [sh] (:stats @sh))

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

(defn- root-of [sh p] (first (filter #(under? % (str p)) (sort-by (comp - count) (keys (:roots @sh))))))

(defn- open-root!
  "The shadow of the repo orig path p is in, cloned on first use."
  [sh p]
  (or (root-of sh p)
      (when-let [{:keys [orig-root repo common]} (origin sh p)]
        (let [clean (str/replace (str (fs/file-name orig-root)) #"[^A-Za-z0-9._-]" "_")
              taken (set (map :shadow (vals (:roots @sh))))
              dest (first (remove #(or (taken %) (fs/exists? %))
                                  (map #(str (fs/path (:base @sh) (if (zero? %) clean (str clean "-" %)))) (range))))
              live-wts (->> (str/split-lines (or (sh! repo "worktree" "list" "--porcelain") ""))
                            (keep #(second (re-find #"^worktree (.+)$" %)))
                            (remove #{repo}))]
          (sh! (:base @sh) "clone" "-q" "--shared" "--no-checkout" repo dest)
          (swap! sh assoc-in [:roots orig-root]
                 {:shadow dest :repo repo :common common :project clean
                  :worktrees (into {} (keep #(when (under? repo %) [(swap-prefix % repo orig-root) nil])) live-wts)})
          orig-root))))

(defn worktree!
  "Note that orig dir wt was a git worktree (a `git worktree add` in a transcript)."
  [sh wt]
  (when-let [r (open-root! sh wt)]
    (when (and (not= r (str wt)) (not (contains? (get-in @sh [:roots r :worktrees]) (str wt))))
      (swap! sh assoc-in [:roots r :worktrees (str wt)] nil))))

(defn- wt-of
  "[orig-root orig-worktree] holding orig path p (the worktree is the root itself when
  p is in no known worktree)."
  [sh p]
  (when-let [r (open-root! sh p)]
    [r (or (first (filter #(under? % (str p)) (sort-by (comp - count) (keys (get-in @sh [:roots r :worktrees])))))
           r)]))

(defn local
  "Where orig path p is in the shadow (p itself outside every shadow)."
  [sh p]
  (if-let [r (and p (root-of sh p))]
    (swap-prefix p r (get-in @sh [:roots r :shadow]))
    p))

(defn orig
  "The inverse of local."
  [sh p]
  (if-let [[r {:keys [shadow]}] (and p (first (filter #(under? (:shadow (val %)) (str p))
                                                      (sort-by (comp - count :shadow val) (:roots @sh)))))]
    (swap-prefix p shadow r)
    p))

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

(defn- history
  "[[epoch-ms sha]] of rev's first-parent history, oldest first."
  [sh repo rev]
  (or (get-in @sh [:cache :history [repo rev]])
      (let [es (vec (reverse (keep #(let [[sha t] (str/split % #" ")] (when t [(* 1000 (parse-long t)) sha]))
                                   (str/split-lines (or (sh! repo "rev-list" "--first-parent" "--format=%H %ct" "--no-commit-header" rev) "")))))]
        (swap! sh assoc-in [:cache :history [repo rev]] es)
        es)))

(defn- merged-tip
  "The tip a deleted branch had when it was merged: 2nd parent of its merge commit."
  [sh repo branch]
  (let [k [:merged repo branch]]
    (if (contains? (:cache @sh) k)
      (get-in @sh [:cache k])
      (let [v (some #(let [[_ p2 s] (str/split % #" " 3)]
                       (when (and p2 (or (str/starts-with? (str s) (str "Merge " branch ":"))
                                         (str/starts-with? (str s) (str "Merge " branch " "))
                                         (= (str s) (str "Merge " branch))
                                         (str/starts-with? (str s) (str "Merge branch '" branch "'"))))
                         p2))
                    (str/split-lines (or (sh! repo "log" "--all" "--merges" "--format=%H %P %s") "")))
            v (when v (second (str/split v #" ")))]
        (swap! sh assoc-in [:cache k] v)
        v))))

(defn- head-at
  "[sha source] of orig worktree wt (in root r) at ts."
  [sh r wt ts branch]
  (let [{:keys [repo common]} (get-in @sh [:roots r])
        wt-repo (if (= r wt) repo (swap-prefix wt r repo))
        admin (when-not (= r wt)
                (some #(let [g (fs/path % "gitdir")]
                         (when (and (fs/exists? g) (= (str/trim (slurp (str g))) (str wt-repo "/.git"))) %))
                      (when (fs/exists? (fs/path common "worktrees")) (fs/list-dir (fs/path common "worktrees")))))
        head-log (if (= r wt) (str (fs/path common "logs" "HEAD")) (some-> admin (fs/path "logs" "HEAD") str))
        branch (when-not (contains? #{nil "" "HEAD"} branch) branch)
        default (some #(when (sh! repo "rev-parse" "-q" "--verify" (str "refs/heads/" %)) %) ["main" "master"])]
    (or (some-> head-log (#(at (reflog sh %) ts)) second (vector :reflog))
        (some-> branch (#(at (reflog sh (str (fs/path common "logs" "refs" "heads" %))) ts)) second (vector :branch))
        (some-> branch (#(or (when (sh! repo "rev-parse" "-q" "--verify" (str "refs/heads/" %)) (str "refs/heads/" %))
                             (merged-tip sh repo %)))
                (#(at (history sh repo %) ts)) second (vector :history))
        (some-> default (#(at (history sh repo %) ts)) second (vector :main)))))

(defn- move! [dir sha]
  (or (sh! dir "checkout" "-q" "--detach" sha)
      (sh! dir "reset" "-q" "--hard" sha)))

(defn sync!
  "Make the shadow worktree holding orig path p be on its commit at ts (epoch ms); dir p
  exists in it when dir? is set. Returns the HEAD source, or nil when p is in no repo."
  [sh p ts branch & {:keys [dir?]}]
  (when-let [[r wt] (wt-of sh p)]
    (let [{:keys [shadow worktrees]} (get-in @sh [:roots r])
          swt (if (= r wt) shadow (or (get worktrees wt) (swap-prefix wt r shadow)))
          [sha src] (head-at sh r wt ts branch)]
      (when sha
        (cond
          (and (= r wt) (not (get-in @sh [:heads swt])))
          (move! swt sha)

          (and (not= r wt) (not (get worktrees wt)))
          (let [tmp (when (and (fs/exists? swt) (seq (fs/list-dir swt)))
                      (let [t (str swt ".pre")] (fs/move swt t) t))]
            (when-not (get-in @sh [:heads shadow])
              (when-let [[s0] (head-at sh r r ts nil)] (move! shadow s0) (swap! sh assoc-in [:heads shadow] s0)))
            (fs/create-dirs (fs/parent swt))
            (fs/delete-if-exists swt)
            (sh! shadow "worktree" "add" "-q" "--detach" swt sha)
            (when tmp (fs/copy-tree tmp swt {:replace-existing true}) (fs/delete-tree tmp))
            (swap! sh assoc-in [:roots r :worktrees wt] swt))

          (not= sha (get-in @sh [:heads swt]))
          (move! swt sha))
        (swap! sh assoc-in [:heads swt] sha)
        (stat! sh src))
      (when dir? (fs/create-dirs (local sh p)))
      src)))

;; --- evidence ------------------------------------------------------------------------

(defn inside? "Is orig path p in a shadow (so it may be written)?" [sh p] (boolean (and p (root-of sh p))))

(defn write! [sh p content]
  (when (inside? sh p)
    (let [f (local sh p)]
      (fs/create-dirs (fs/parent f))
      (spit f content)
      true)))

(defn delete! [sh p] (when (inside? sh p) (fs/delete-if-exists (local sh p))))

(defn content [sh p] (let [f (local sh p)] (when (and f (fs/regular-file? f)) (slurp f))))
