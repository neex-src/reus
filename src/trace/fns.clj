(ns trace.fns
  "The fn layer: Clojure definitions as a history of their own beside the trace, every version a
  commit with two parents (monkeymob's double parent). Derived from the trace commits and never in
  the recorder's way; made again from scratch it has the same shas.

  Refs. refs/fn/<project>/<branch> for every trace branch refs/heads/<project>/<branch> (main,
  s/<s8>, a/<s8>-<agent>): the fn state of its tip. Outside refs/heads, so the trace's readers never
  see them; `git log fn/reus/main` finds them.

  Tree. Every Clojure file (.clj .cljs .cljc .bb) of the trace commit is a dir of its top-level
  definitions, one blob per name with the form's text: src/trace/record.clj/on-tool, .../ns, `area
  :circle` for a defmethod, `x~2` for a second def of x (a `/` in a name is `%2F`). Other forms
  are left out. A file rewrite-clj cannot parse is cut where a line starts with `(`.

  Commits, only where a definition changed. Parent 1: the previous fn commit of the line (the fn
  state of the trace commit's first parent; a project's first one has an empty root, `fn layer of
  <project>`). Parent 2: the trace commit (the tool call, its prompt, the whole worktree). A
  mirrored real merge also gets the fn commits of the merged session and agent tips. Subject: what
  changed, by file (+ added, - removed); body: the trace commit's subject, trailers Fn-Added /
  Fn-Changed / Fn-Removed (at most 30, then Fn-More), Prompt-Id, Tool, Agent. Author, committer
  and dates are the trace commit's.

    git log --first-parent -p fn/reus/main -- src/trace/record.clj/on-tool   one fn, version by version
    git log -p fn/reus/main -- src/trace/record.clj/on-tool                  ... with every session edit merged in
    git show <fn commit>^2                                                  the call that made it

  Catch-up per project from the trace heads seen last (.git/trace-fns/<project>), the new trace
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
       (str/join (map #(str "%x1f%(trailers:key=" % ",valueonly,separator=%x2C)") ["Prompt-Id" "Tool" "Agent"]))))

(defn- new-commits
  "The trace commits reachable from heads and not from done, parents first:
  [{:sha :parents :author :committer :subject :prompt-id :tool :agent}]."
  [heads done]
  (let [done (g/have (vals done))]
    (for [rec (rest (str/split (apply git! "log" "--topo-order" "--reverse" "--date=raw" (str "--format=" fmt)
                                      (concat (distinct (vals heads)) (when (seq done) (cons "--not" done))))
                               #"\u001e"))
          :let [[sha ps author committer subject pid tool agent] (map str/trim (str/split rec #"\u001f" -1))]]
      {:sha sha :parents (vec (remove str/blank? (str/split ps #" "))) :author author :committer committer
       :subject subject :prompt-id pid :tool tool :agent agent})))

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

(defn- message [ch {:keys [subject prompt-id tool agent]}]
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
                                             [["Prompt-Id" prompt-id] ["Tool" tool] ["Agent" agent]]))))}))

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

(defn- catch-up!
  "The fn commits of project's new trace commits (commits, parents first) in store s, and its fn refs
  pointed at the fn state of each head; how many fn commits."
  [s project heads commits]
  (let [index (delay (repo-index project))
        fnof (atom {}) ; trace sha -> fn id (nil: no definitions yet), this run's
        earlier (memoize #(fn-at-repo @index %))
        at* #(if (contains? @fnof %) (@fnof %) (earlier %))
        root (delay (store/commit! s {:parents [] :tree {:base nil} :subject (str "fn layer of " project) :body ""
                                      :author root-ident :committer root-ident}))
        made (atom 0)]
    (doseq [chunk (partition-all 500 commits)]
      (let [ds (diffs chunk)
            texts (g/cat-batch (vec (distinct (remove #{zero} (mapcat (fn [[o nw]] [o nw]) (apply concat (vals ds)))))))
            defs (memoize (fn [sha] (if (= sha zero) {} (explode (get texts sha "")))))]
        (doseq [{:keys [sha parents author committer] :as c} chunk]
          (let [prev (some-> (first parents) at*)
                ch (changes (get ds sha) defs)]
            (swap! fnof assoc sha
                   (if (empty? ch)
                     prev
                     (let [extras (distinct (remove #(or (nil? %) (= % prev)) (map at* (rest parents))))]
                       (swap! made inc)
                       (store/commit! s (merge (message ch c)
                                               {:parents (vec (concat [(or prev @root) sha] extras))
                                                :tree {:keep true :changes (into {} (map (fn [[pth _ text]] [pth text]) ch))}
                                                :author author :committer committer})))))))))
    (doseq [[r sha] heads :let [f (at* sha)] :when f]
      (store/advance! s (fn-ref r) f))
    @made))

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
                        :when (not= hs done)]
                    [project hs (vec (new-commits hs done))]))
        n (reduce + (map (comp count last) todo))
        batch? (> n batch-over)
        s (if batch? (store/batch repo) (store/live repo))
        made (try (reduce + (for [[project hs commits] todo] (catch-up! s project hs commits)))
                  (finally (when batch? (store/close! s))))]
    (doseq [[project hs] todo] (files/kv-write! repo :fns project hs))
    {:commits n :made made}))

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
                  :extra-env {"TRACE_REPO" (str repo)}}
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
