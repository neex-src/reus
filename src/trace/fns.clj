(ns trace.fns
  "The fn layer: Clojure definitions as a history of their own beside the trace, every version a
  commit with two parents (monkeymob's double parent). Derived from the trace commits and never in
  the recorder's way; made again from scratch it has the same shas.

  Refs, outside refs/heads so the trace's readers never see them (`git log fn/reus/main` finds them):
    refs/fn/<project>/main                    the fn state of trace main
    refs/fn/<project>/s/<s8>/<worktree>       a session's line in each worktree it worked in (the
    refs/fn/<project>/a/<s8>-<agent>/<worktree>  dir's name; `_`: commits without a Worktree trailer)
  A session that works in a worktree and writes in the main checkout flips its trace branch between
  the two trees; each worktree keeps a line of its own, the first time seen it starts from the line
  closest to it.

  Tree. Every Clojure file (.clj .cljs .cljc .bb) of the trace commit is a dir of its top-level
  definitions, one blob per name with the form's text: src/trace/record.clj/on-tool, .../ns, `area
  :circle` for a defmethod, `x~2` for a second def of x (a `/` in a name is `%2F`). Other forms
  are left out. A file rewrite-clj cannot parse is cut where a line starts with `(`.

  Commits, only where a definition changed. Parent 1: the previous fn commit of the line (a project's
  first one has an empty root, `fn layer of <project>`). Parent 2: the trace commit (the tool call, its
  prompt, the whole worktree). Then the session's other lines that already had a changed definition
  as it is now (a merge in the main checkout of what a worktree made), and for a mirrored real merge
  the fn states of the merged session and agent tips. Subject: what
  changed, by file (+ added, - removed); body: the trace commit's subject, trailers Fn-Added /
  Fn-Changed / Fn-Removed (at most 30, then Fn-More), Prompt-Id, Tool, Agent, Worktree. Author, committer
  and dates are the trace commit's.

    git log --first-parent -p fn/reus/main -- src/trace/record.clj/on-tool   one fn, version by version
    git log -p fn/reus/main -- src/trace/record.clj/on-tool                  ... into the sessions that made it
    git show <fn commit>^2                                                  the call that made it

  Catch-up per project from the trace heads and their lines seen last (.git/trace-fns/<project>), the new trace
  commits in topological order: a few through the live store, more (a backfill) through fast-import.
  One run at a time (.git/trace-fns.lock). A trace repo has a fn layer from its first `bb trace:fns
  [repo]` on (the backfill); from then the worker starts a catch-up whenever it goes idle."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as parser]
            [trace.files :as files]
            [trace.git :as g :refer [git!]]
            [trace.health :as health]
            [trace.store :as store]))

(def pathspec ["*.clj" "*.cljs" "*.cljc" "*.bb"])
(def max-bytes "A bigger file is no definitions (generated code)." (* 1024 1024))
(def batch-over "More new trace commits than this go through fast-import." 200)
(def max-trailers 30)
(def zero "0000000000000000000000000000000000000000")
(def root-ident "reus-trace <trace@reus.local> 1700000000 +0000")

(defn- lines [s] (remove str/blank? (str/split-lines (str s))))

;; --- definitions ------------------------------------------------------------------------

(defn- form-name
  "What a top-level form defines from its first words: `ns`, the name of a def-like form (a head
  whose name starts with def), a defmethod's with its dispatch value; else nil."
  [head nm dispatch]
  (let [local (str/replace (str head) #"^.*/" "")]
    (cond (= head "ns") "ns"
          (and nm (str/starts-with? local "def"))
          (str nm (when (= local "defmethod") (some->> dispatch (str " ")))))))

(defn- unmeta [nd]
  (if (#{:meta :meta*} (n/tag nd)) (recur (last (remove n/whitespace-or-comment? (n/children nd)))) nd))

(defn- node-name [nd]
  (when (= :list (n/tag nd))
    (let [[h nm d] (map unmeta (remove n/whitespace-or-comment? (n/children nd)))]
      (when (and h (= :token (n/tag h)))
        (form-name (n/string h) (some-> nm n/string) (some-> d n/string))))))

(defn- parsed [s]
  (try (doall (keep #(when-let [nm (node-name %)] [nm (n/string %)]) (n/children (parser/parse-string-all s))))
       (catch Exception _ nil)))

(defn- by-lines
  "The forms of a file that does not parse: one starts at each line that starts with `(`."
  [s]
  (let [ls (vec (str/split-lines s))
        starts (keep-indexed (fn [i l] (when (str/starts-with? l "(") i)) ls)]
    (keep (fn [[a b]]
            (let [text (str/join "\n" (reverse (drop-while #(re-matches #"\s*(;.*)?" %) (rseq (subvec ls a b)))))
                  [_ h nm d] (re-find #"^\((\S+)\s+(?:\^\S+\s+)*([^\s()\[\]{}\"]+)(?:\s+([^\s()\[\]{}\"]+))?" text)]
              (when-let [nm (form-name h nm d)] [nm text])))
          (partition 2 1 (concat starts [(count ls)])))))

(defn- entry-name [nm]
  (let [s (str/replace (str/trim (str/replace nm #"\s+" " ")) "/" "%2F")
        s (if (> (count s) 120) (subs s 0 120) s)]
    (if (#{"" "." ".."} s) (str "%" s) s)))

(defn explode
  "{name text} of the top-level definitions of Clojure source s (see ns)."
  [s]
  (if (> (count s) max-bytes)
    {}
    (loop [[[nm text] & more :as forms] (or (parsed s) (by-lines s)) seen {} out {}]
      (if (empty? forms)
        out
        (let [k (entry-name nm)
              i (inc (seen k 0))]
          (recur more (assoc seen k i) (assoc out (if (= i 1) k (str k "~" i)) (str text "\n"))))))))

;; --- the trace side -----------------------------------------------------------------------

(defn- heads
  "{project {ref sha}} of the trace branches."
  []
  (reduce (fn [m l]
            (let [[r sha] (str/split l #" ")]
              (if-let [[_ project] (re-matches #"refs/heads/([^/]+)/(?:main|s/[^/]+|a/[^/]+)" r)]
                (assoc-in m [project r] sha)
                m)))
          {} (lines (git! "for-each-ref" "--format=%(refname) %(objectname)" "refs/heads/"))))

(def ^:private fmt
  (str "%x1e%H%x1f%P%x1f%an <%ae> %ad%x1f%cn <%ce> %cd%x1f%s"
       (str/join (map #(str "%x1f%(trailers:key=" % ",valueonly,separator=%x2C)") ["Prompt-Id" "Tool" "Agent" "Worktree" "Real-Commit"]))))

(defn- new-commits
  "The trace commits reachable from heads and not from done, parents first:
  [{:sha :parents :author :committer :subject :prompt-id :tool :agent :worktree :main?}]."
  [heads done]
  (let [done (g/have (vals done))]
    (for [rec (rest (str/split (apply git! "log" "--topo-order" "--reverse" "--date=raw" (str "--format=" fmt)
                                      (concat (distinct (vals heads)) (when (seq done) (cons "--not" done))))
                               #"\u001e"))
          :let [[sha ps author committer subject pid tool agent wt real] (map str/trim (str/split rec #"\u001f" -1))]]
      {:sha sha :parents (vec (remove str/blank? (str/split ps #" "))) :author author :committer committer
       :subject subject :prompt-id pid :tool tool :agent agent :worktree wt
       :main? (or (not (str/blank? real)) (str/blank? ps))})))

(defn- diffs
  "{sha [[old-blob new-blob path]...]} of the Clojure files each commit changed against its first parent."
  [commits]
  (let [out (apply git! "-c" "core.quotePath=false" "diff-tree" "--stdin" "-r" "--root" "--no-renames"
                   (concat ["--"] pathspec
                           [{:in (str/join (map (fn [{:keys [sha parents]}] (str sha (some->> (first parents) (str " ")) "\n")) commits))}]))]
    (loop [[l & more] (str/split-lines out) cur nil acc {}]
      (cond (nil? l) acc
            (re-matches #"[0-9a-f]{40}" l) (recur more l acc)
            (not (str/starts-with? l ":")) (recur more cur acc)
            :else (let [[meta path] (str/split l #"\t" 2)
                        [om nm o nw] (str/split (subs meta 1) #" ")]
                    (recur more cur (if (and cur (every? #{"100644" "100755" "000000"} [om nm]) (not (str/starts-with? path "\"")))
                                      (update acc cur (fnil conj []) [o nw path])
                                      acc)))))))

(defn- changes
  "[[fn-path kind text-or-nil]] of the definitions that differ between the old and new version of each
  file; defs: blob sha -> {name text}."
  [entries defs]
  (for [[old new path] entries
        :let [a (defs old) b (defs new)]
        nm (sort (distinct (concat (keys a) (keys b))))
        :when (not= (a nm) (b nm))]
    [(str path "/" nm) (cond (nil? (a nm)) :added (nil? (b nm)) :removed :else :changed) (b nm)]))

(defn- message
  "Subject and body of a fn commit with changes ch for trace commit c."
  [ch {:keys [subject prompt-id tool agent worktree]}]
  (let [by-file (sort-by key (group-by #(str/replace (first %) #"/[^/]*$" "") ch))
        mark {:added "+" :removed "-" :changed ""}
        subj (str "fn " (str/join "; " (for [[f xs] by-file]
                                        (str (fs/file-name f) ": " (str/join ", " (map (fn [[pth k]] (str (mark k) (fs/file-name pth))) xs))))))
        label {:added "Fn-Added" :changed "Fn-Changed" :removed "Fn-Removed"}
        shown (take max-trailers ch)]
    {:subject (if (> (count subj) 120) (str (subs subj 0 117) "...") subj)
     :body (str subject "\n\n"
                (str/join "\n" (concat (map (fn [[pth k]] (str (label k) ": " pth)) shown)
                                       (when (> (count ch) max-trailers) [(str "Fn-More: " (- (count ch) max-trailers))])
                                       (keep (fn [[k v]] (when-not (str/blank? v) (str k ": " v)))
                                             [["Prompt-Id" prompt-id] ["Tool" tool] ["Agent" agent] ["Worktree" worktree]]))))}))

;; --- fn commits of earlier runs --------------------------------------------------------------

(defn- repo-index
  "{trace sha fn sha} of project's fn commits in the repo (parent 2 of a fn commit is its trace commit)."
  [project]
  (let [tips (lines (git! "for-each-ref" "--format=%(objectname)" (str "refs/fn/" project "/")))]
    (if (empty? tips)
      {}
      (into {} (keep #(let [[f _ ctx] (str/split % #" ")] (when ctx [ctx f]))
                     (lines (apply git! "rev-list" "--first-parent" "--parents" tips)))))))

(defn- fn-at-repo
  "The fn commit of trace commit c made by an earlier run: that of the nearest first-parent ancestor
  (c itself included) that has one; nil: none."
  [index c]
  (when (seq index)
    (let [proc (p/process {:dir g/*repo* :err :string} "git" "rev-list" "--first-parent" c)]
      (try (with-open [r (io/reader (:out proc))] (some index (line-seq r)))
           (finally (p/destroy proc))))))

;; --- catching up ------------------------------------------------------------------------------

(defn- fn-ref [head] (str "refs/fn/" (subs head (count "refs/heads/"))))

(defn- clj-files
  "{path blob} of the Clojure files of a commit's tree (as diffs sees them)."
  [rev]
  (into {} (keep #(let [[meta path] (str/split % #"\t" 2) [mode _ sha] (str/split meta #" ")]
                    (when (and (#{"100644" "100755"} mode) (re-find #"\.(clj|cljs|cljc|bb)$" path)
                               (not (re-find #"[\"\\\u0000-\u001f\u007f]" path)))
                      [path sha]))
                 (remove str/blank? (str/split (git! "ls-tree" "-r" "-z" rev) #"\u0000")))))

(defn- between
  "Diff entries [[old new path]] from one {path blob} to another."
  [a b]
  (for [p (sort (distinct (concat (keys a) (keys b)))) :when (not= (a p) (b p))]
    [(get a p zero) (get b p zero) p]))

(defn- fn-of [lines] (get-in lines [:by (:cur lines) :fn]))

(defn- catch-up!
  "The fn commits of project's new trace commits (commits, parents first) in store s, and its fn refs;
  {:made n :lines {ref lines}}. The lines of a trace commit, {:cur worktree :by {worktree {:fn id :files
  {path blob}}}}, are the fn state each worktree had last on its first-parent line (\"=main\" for trace
  main, \"\" for session commits without a Worktree trailer); :cur is the one it holds. A worktree seen for the
  first time starts from the line closest to it. done: the last run's {:heads {ref sha} :lines {ref
  {:cur w :by {w fn-sha}}}}."
  [s project heads commits done]
  (let [index (delay (repo-index project))
        earlier (memoize #(fn-at-repo @index %))
        mine (atom {}) ; trace sha -> lines, this run's
        restore (fn [c] ; the lines of a trace commit of an earlier run
                  (let [stored (some (fn [[r sha]] (when (= sha c) (get-in done [:lines r]))) (:heads done))
                        ctx-files #(if-let [ctx (second (str/split (git! "log" "-1" "--format=%P" %) #" "))] (clj-files ctx) {})]
                    (if stored
                      {:cur (:cur stored)
                       :by (into {} (for [[w f] (:by stored)]
                                      [w {:fn f :files (if (= w (:cur stored)) (clj-files c) (ctx-files f))}]))}
                      {:cur "=main" :by {"=main" {:fn (earlier c) :files (clj-files c)}}})))
        lines-of (fn [c] (or (@mine c) (let [l (restore c)] (swap! mine assoc c l) l)))
        fn-at #(if-let [l (@mine %)] (fn-of l) (earlier %))
        root (delay (store/commit! s {:parents [] :tree {:base nil} :subject (str "fn layer of " project) :body ""
                                      :author root-ident :committer root-ident}))
        made (atom 0)]
    (doseq [chunk (partition-all 500 commits)]
      (let [ds (diffs chunk)
            texts (g/cat-batch (vec (distinct (remove #{zero} (mapcat (fn [[o nw]] [o nw]) (apply concat (vals ds)))))))
            defs (memoize (fn [sha] (if (= sha zero) {} (explode (or (get texts sha) (get (g/cat-batch [sha]) sha ""))))))
            text-in (fn [files fn-path] ; a definition's text in a line, by its fn path
                      (let [i (str/last-index-of fn-path "/")]
                        (get (defs (get files (subs fn-path 0 i) zero)) (subs fn-path (inc i)))))]
        (doseq [{:keys [sha parents author committer worktree main?] :as c} chunk]
          (let [l (if-let [p1 (first parents)] (lines-of p1) {:cur "" :by {"" {:fn nil :files {}}}})
                from (get-in l [:by (:cur l)])
                entries (get ds sha)
                state (reduce (fn [m [_ nw path]] (if (= nw zero) (dissoc m path) (assoc m path nw))) (:files from) entries)
                w (cond (seq worktree) worktree main? "=main" :else "")
                line (or (get-in l [:by w])
                         (when (seq entries) ; a first visit: from the line that is closest to it
                           (apply min-key #(count (changes (between (:files %) state) defs))
                                  (cons from (vals (dissoc (:by l) (:cur l))))))
                         from)
                ch (if (identical? line from) (changes entries defs) (changes (between (:files line) state) defs))
                f (if (seq ch)
                    (let [others (vals (dissoc (:by l) w (:cur l)))
                          came-from (for [o (cons from others) :when (not (identical? o line))
                                          :when (some (fn [[pth _ text]] (and text (= text (text-in (:files o) pth)))) ch)]
                                      (:fn o))
                          extras (distinct (remove #(or (nil? %) (= % (:fn line))) (concat came-from (map fn-at (rest parents)))))]
                      (swap! made inc)
                      (store/commit! s (merge (message ch c)
                                              {:parents (vec (concat [(or (:fn line) @root) sha] extras))
                                               :tree {:keep true :changes (into {} (map (fn [[pth _ text]] [pth text]) ch))}
                                               :author author :committer committer})))
                    (:fn line))]
            (swap! mine assoc sha {:cur w :by (assoc (:by l) w {:fn f :files state})})))))
    (doseq [[r sha] heads
            :when (not= sha (get-in done [:heads r])) ; a head that did not move has its refs
            :let [main? (str/ends-with? r "/main")
                  l (when-not main? (lines-of sha))
                  named (if main?
                          {(fn-ref r) (fn-at sha)}
                          (let [ws (remove #{"=main"} (keys (:by l)))
                                nm #(if (= "" %) "_" (files/clean-key (fs/file-name %)))
                                dup (set (keep (fn [[k v]] (when (> (count v) 1) k)) (group-by nm ws)))]
                            (into {} (for [w ws :let [n (nm w)]]
                                       [(str (fn-ref r) "/" (if (dup n) (str n "-" (subs (store/blob-sha w) 0 6)) n))
                                        (get-in l [:by w :fn])]))))]
            [ref f] named :when f]
      (store/advance! s ref f))
    {:made @made
     :lines (into {} (for [[r sha] heads :when (not= sha (get-in done [:heads r])) :let [l (lines-of sha)]]
                       [r {:cur (:cur l) :by (into {} (for [[w {:keys [fn]}] (:by l) :when fn] [w (store/sha-of s fn)]))}]))}))

(defn- lock-file [repo] (str (fs/path repo ".git" "trace-fns.lock")))
(defn- state-dir [repo] (fs/path repo ".git" "trace-fns"))

(defn on?
  "Does repo have a fn layer? From its first `bb trace:fns` on (the backfill), the worker keeps it up."
  [repo] (fs/directory? (state-dir repo)))

(defn- once!
  "One catch-up of every project whose trace heads moved: {:commits looked at :made fn commits}."
  [repo]
  (let [todo (vec (for [[project hs] (heads)
                        :let [done (files/kv-read repo :fns project)]
                        :when (not= hs (:heads done))]
                    [project hs done (vec (new-commits hs (:heads done)))]))
        n (reduce + (map (comp count last) todo))
        batch? (> n batch-over)
        s (if batch? (store/batch repo) (store/live repo))
        rs (try (doall (for [[project hs done commits] todo] (catch-up! s project hs commits done)))
                (finally (when batch? (store/close! s))))]
    (doseq [[[project hs done] r] (map vector todo rs)]
      (files/kv-write! repo :fns project {:heads hs :lines (merge (select-keys (:lines done) (keys hs)) (:lines r))}))
    {:commits n :made (reduce + (map :made rs))}))

(defn run!
  "Catch the fn layer of repo up with its trace, again while the trace moved meanwhile:
  {:commits :made}; nil when another run holds the lock."
  [repo]
  (binding [g/*repo* (str repo)]
    (files/try-file-lock (lock-file repo)
                         #(do (fs/create-dirs (state-dir repo))
                              (loop [acc {:commits 0 :made 0}]
                                (let [r (once! repo)]
                                  (if (pos? (:commits r)) (recur (merge-with + acc r)) acc)))))))

(defn wake!
  "Start a catch-up of repo, detached, unless one is running (the worker calls this when idle)."
  [repo]
  (when (files/try-file-lock (lock-file repo) (constantly true))
    (let [log (io/file (str (fs/path repo ".git" "trace-worker.log")))]
      (p/process {:in "" :out :append :out-file log :err :append :err-file log
                  :dir (str repo) :extra-env {"TRACE_REPO" (str repo)}}
                 "setsid" "-f" g/bb "--config" (str (fs/path g/home "bb.edn")) "-m" "trace.fns" "--quiet"))))

(defn -main
  "bb trace:fns [repo] [--quiet]: catch up, say how much."
  [& args]
  (let [repo (or (first (remove #(str/starts-with? % "--") args)) g/*repo*)]
    (binding [g/*repo* (str repo)]
      (try
        (let [r (run! repo)]
          (health/ok! :fns)
          (when-not (some #{"--quiet"} args)
            (println (if r (str (:commits r) " trace commits looked at, " (:made r) " fn commits made")
                         "a catch-up is running already"))))
        (catch Throwable e
          (health/log! "fns" "" {} e)
          (health/fail! :fns "" {} e)
          (when-not (some #{"--quiet"} args) (binding [*out* *err*] (println "fns failed:" (ex-message e))))
          (System/exit 1))))))
