(ns trace.fnlog
  "The fn layer (trace.fns) read back for trace.gui: every version of every definition of a project.

  One git log over refs/fn/<project>/*, --raw --diff-merges=first-parent, so a fn commit lists what
  changed against the version before it on its line; the trace commits (parent 2 and below) are cut
  off with --not --branches and --grep '^fn '. Cached by the fn refs' tips.

  A version: {:sha :ct :subject (the fn commit's) :trace (parent 2) :trace-subject :tool :agent
  :worktree :main? (made on main: a mirrored commit or merge) :status A|M|D :old :new (blob shas) :path}.
  Where it was made: (where versions) names each session version's trace commit against the
  project's session and agent branches (git name-rev)."
  (:require [clojure.string :as str]
            [trace.git :as g :refer [git]]))

(def zero (apply str (repeat 40 "0")))

(defn fn-refs
  "The fn refs of project m."
  [m]
  (remove str/blank? (str/split-lines (git "for-each-ref" "--format=%(refname) %(objectname)" (str "refs/fn/" m "/")))))

(defn on?
  "Has project m a fn layer?"
  [m]
  (boolean (and m (seq (git "for-each-ref" "--count=1" "--format=%(refname)" (str "refs/fn/" m "/"))))))

(defn- trailer [body k] (some->> (re-seq (re-pattern (str "(?m)^" k ": (.*)$")) body) last second))

(defn- commit [head]
  (let [[sha ps ct body] (str/split head #"\u0001" 4)
        ps (str/split ps #" ")
        lines (str/split-lines (str body))]
    {:sha sha :ct (parse-long ct) :subject (first lines)
     :trace (second ps) :trace-subject (nth lines 2 nil)
     :tool (trailer body "Tool") :agent (trailer body "Agent") :worktree (trailer body "Worktree")
     :main? (nil? (trailer body "Tool"))}))

(defn- parse
  "git log -z --raw output: [commit {:changes [...]}...], newest first."
  [out]
  (loop [[t & ts] (str/split out #"\u0000") cur nil acc []]
    (if (nil? t)
      (cond-> acc cur (conj cur))
      (let [t (str/replace t #"^\n+" "")]
        (cond
          (str/starts-with? t "@")
          (recur ts (assoc (commit (subs t 1)) :changes []) (cond-> acc cur (conj cur)))
          (and cur (str/starts-with? t ":"))
          (let [[_ _ old new st] (str/split (subs t 1) #" ")]
            (recur (rest ts) (update cur :changes conj {:old old :new new :status (subs st 0 1) :path (first ts)}) acc))
          :else (recur ts cur acc))))))

(def ^:private cache (atom {}))

(defn history
  "Every fn commit of project m with its changes, newest first (cached by the fn refs' tips)."
  [m]
  (let [refs (fn-refs m)
        k [g/*repo* m refs]]
    (or (get @cache k)
        (let [h (if (empty? refs)
                  []
                  (parse (apply git "log" "-z" "--raw" "--no-abbrev" "--diff-merges=first-parent" "--grep=^fn "
                                "--format=@%H%x01%P%x01%ct%x01%B"
                                (concat (map #(first (str/split % #" ")) refs) ["--not" "--branches"]))))]
          (reset! cache {k h})
          h))))

(defn index
  "{path {:n texts :last ct}} of every definition project m ever had: :n the distinct texts it had
  (a text that comes back, as in the flips of old traces or on another line, counts once), :last
  when its newest text first appeared."
  [m]
  (let [seen (reduce (fn [acc {:keys [ct changes]}]
                       (reduce (fn [acc {:keys [path new status]}]
                                 (if (or (= "D" status) (get-in acc [path new])) acc (assoc-in acc [path new] ct)))
                               acc changes))
                     {} (reverse (history m)))]
    (into {} (map (fn [[path bs]] [path {:n (count bs) :last (apply max (vals bs))}])) seen)))

(defn on-main
  "The definition paths in project m's fn main now."
  [m]
  (let [{:keys [exit out]} (g/git* {} "ls-tree" "-r" "-z" "--name-only" (str "refs/fn/" m "/main"))]
    (if (zero? exit) (set (remove str/blank? (str/split out #"\u0000"))) #{})))

(defn versions
  "The versions of one definition (path <file>/<name>) in project m, oldest first."
  [m path]
  (reverse (for [c (history m) ch (:changes c) :when (= path (:path ch))]
             (merge (dissoc c :changes) ch))))

(defn unflip
  "Versions with the flips of old traces folded: a removal followed on the same branch (br: version ->
  branch) by an addition is one change from the removed text to the added one, or nothing when the
  text came back the same. Returns [versions flips-folded]."
  [br vs]
  (loop [[v & more :as vs] vs out [] n 0]
    (cond
      (nil? v) [out n]
      (and (= "D" (:status v)) (= "A" (:status (first more))) (= (br v) (br (first more))))
      (let [a (first more)]
        (recur (rest more) (if (= (:old v) (:new a)) out (conj out (assoc a :status "M" :old (:old v)))) (inc n)))
      :else (recur more (conj out v) n))))

(defn- name-rev [m kind shas]
  (if (empty? shas)
    {}
    (let [out (apply git "name-rev" "--name-only" (str "--refs=refs/heads/" m "/" kind "/*") shas)]
      (into {} (keep (fn [[sha n]] (when-let [b (second (re-find #"^(?:refs/heads/)?([^~^]+)" n))]
                                     (when (not= b "undefined") [sha b]))))
            (map vector shas (str/split-lines out))))))

(defn where
  "{trace sha branch} for the session versions: the session branch whose history holds the trace
  commit, the agent branch for an agent's version (an agent branch holds its session's history too);
  nil when none does any more."
  [m vs]
  (let [{agent true session false} (group-by (comp boolean :agent) (filter :tool vs))
        s (name-rev m "s" (distinct (map :trace session)))
        a (name-rev m "a" (distinct (map :trace agent)))]
    (merge s a (name-rev m "s" (remove a (distinct (map :trace agent)))))))
