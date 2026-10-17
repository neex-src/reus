(ns trace.ignore
  "gitignore, for a replay that has no checkout to ask `git check-ignore` about.

  (parse text)                  the rules of a .gitignore / info/exclude / global excludes file
  (ignored? base dir-rules rel) is the path rel (relative to the worktree root, no leading /)
                                ignored? base: rules of the lowest precedence (global excludes,
                                info/exclude, in this order); dir-rules: (fn [dir]) the rules of
                                the .gitignore in dir (\"\" is the root), or nil.
  A path is ignored when it or one of its parent directories is; a directory the rules ignore
  cannot have anything re-included inside, as in git."
  (:require [clojure.string :as str])
  (:import [java.util.regex Pattern]))

(defn- glob->regex
  "The regex source of a gitignore glob (no slash handling: the caller anchors it)."
  [g]
  (let [n (count g)]
    (loop [i 0 sb (StringBuilder.)]
      (if (>= i n)
        (str sb)
        (let [c (.charAt g i)
              nxt (when (< (inc i) n) (.charAt g (inc i)))]
          (cond
            (and (= c \*) (= nxt \*))
            (let [pre-slash? (or (zero? i) (= \/ (.charAt g (dec i))))
                  post (when (< (+ i 2) n) (.charAt g (+ i 2)))]
              (cond
                (and pre-slash? (= post \/)) (recur (+ i 3) (.append sb "(?:.*/)?"))
                (and pre-slash? (nil? post)) (recur (+ i 2) (.append sb ".*"))
                :else (recur (+ i 2) (.append sb "[^/]*"))))
            (= c \*) (recur (inc i) (.append sb "[^/]*"))
            (= c \?) (recur (inc i) (.append sb "[^/]"))
            (= c \\) (if nxt (recur (+ i 2) (.append sb (Pattern/quote (str nxt)))) (recur (inc i) sb))
            (= c \[) (if-let [j (str/index-of g "]" (+ i 2))]
                       (let [body (subs g (inc i) j)
                             body (if (str/starts-with? body "!") (str "^" (subs body 1)) body)]
                         (recur (inc j) (.append sb (str "[" (str/replace body "\\" "\\\\") "]"))))
                       (recur (inc i) (.append sb (Pattern/quote "["))))
            :else (recur (inc i) (.append sb (Pattern/quote (str c))))))))))

(defn- rule [line]
  (let [l (str/replace line #"(?<!\\)\s+$" "")]
    (when-not (or (str/blank? l) (str/starts-with? l "#"))
      (let [neg? (and (str/starts-with? l "!") (not (str/starts-with? l "!!")))
            l (cond neg? (subs l 1) :else l)
            dir? (str/ends-with? l "/")
            l (if dir? (subs l 0 (dec (count l))) l)
            anchored? (str/includes? l "/")
            l (str/replace l #"^/" "")]
        (when-not (str/blank? l)
          {:neg? neg? :dir? dir? :anchored? anchored? :re (re-pattern (str "^" (glob->regex l) "$"))})))))

(def ^:private parsed (atom {}))

(defn parse [text]
  (or (get @parsed text)
      (let [r (vec (keep rule (str/split-lines (str text))))]
        (when (> (count @parsed) 500) (reset! parsed {}))
        (swap! parsed assoc text r)
        r)))

(defn- verdict
  "What rules say of path rel (relative to the rules' dir): :ignore, :keep or nil. The last
  rule that matches wins."
  [rules rel is-dir?]
  (let [base (last (str/split rel #"/"))]
    (some (fn [{:keys [neg? dir? anchored? re]}]
            (when (and (or is-dir? (not dir?)) (re-find re (if anchored? rel base)))
              (if neg? :keep :ignore)))
          (rseq rules))))

(defn- ancestors
  "Directories of rel, outermost first: a/b/c -> a, a/b."
  [rel]
  (let [parts (str/split rel #"/")]
    (map #(str/join "/" (take % parts)) (range 1 (count parts)))))

(defn- dir-of [rel] (let [i (str/last-index-of rel "/")] (if i (subs rel 0 i) "")))

(defn- one?
  "Is exactly this path (a dir, or a file) ignored by the rules in force for it?"
  [base dir-rules rel is-dir?]
  (let [dirs (reverse (cons "" (ancestors rel)))] ; deepest first
    (or (some (fn [d]
                (when-let [rules (dir-rules d)]
                  (verdict rules (if (= d "") rel (subs rel (inc (count d)))) is-dir?)))
              (filter #(or (= % "") (str/starts-with? rel (str % "/"))) dirs))
        (some #(verdict % rel is-dir?) (reverse base)))))

(defn ignored?
  "See ns."
  [base dir-rules rel]
  (boolean (or (some #(= :ignore (one? base dir-rules % true)) (ancestors rel))
               (= :ignore (one? base dir-rules rel false)))))
