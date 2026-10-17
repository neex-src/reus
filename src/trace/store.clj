(ns trace.store
  "Everything trace.record reads or writes in the trace repo, behind one protocol, so the
  live hook and a replay can differ in how, not in what.

  Ids. A ref name is a full one (refs/heads/x); a blob or commit id is whatever the store
  hands out (a sha live; a fast-import mark in a batch) and is only passed back to it.
  A commit is {:parents [id...] :base id|nil :files {path content} :subject :body}: its
  tree is base's plus files, its first parent is parents[0]. Content is a string or bytes.

  Key/value state (kv): namespaces :sessions :last :pending (edn values) and :prev (a
  string); a nil value deletes. Live they are the files .git/trace-<ns>/<key>.

  (live repo)   plumbing and files in the repo, as the hook has always done: commits built
                with a private index and commit-tree, refs by update-ref, one mkdir lock.
  (batch repo)  a replay's store: state in memory, objects streamed to one `git fast-import`,
                blob shas computed in-process, commits addressed by marks; notes and refs are
                written by (close! s). (at! s ms) sets the date of what follows."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.git :as g :refer [git]])
  (:import [java.io BufferedOutputStream ByteArrayInputStream OutputStream]
           [java.security MessageDigest]
           [java.time Instant ZoneId]))

(defprotocol Store
  (ensure! [s] "Create the repo when it is not there.")
  (locked [s f] "Call f holding the store's lock.")
  (tip [s ref] "The id a full ref points to, or nil.")
  (refs [s prefix] "The full names of the refs that start with prefix.")
  (has? [s rev path] "Does commit rev have path in its tree?")
  (blob! [s content] "Store content; its id.")
  (commit! [s c] "Make a commit (see ns); its id.")
  (advance! [s ref id] "Point a full ref at id.")
  (note! [s ref id content] "Keep content as the note of commit id in notes ref.")
  (kv [s ns k] "The value of k in ns, or nil.")
  (kv! [s ns k v] "Set k in ns; nil deletes.")
  (kv-all [s ns] "{k v} of ns."))

(defn clean-key [k] (str/replace (str k) #"[^A-Za-z0-9._-]" "_"))

(defn bytes-of ^bytes [content]
  (if (bytes? content) content (.getBytes (str content) "UTF-8")))

(defn blob-sha
  "The git sha of content as a blob."
  [content]
  (let [bs (bytes-of content)
        md (doto (MessageDigest/getInstance "SHA-1")
             (.update (.getBytes (str "blob " (alength bs) "\u0000") "UTF-8"))
             (.update bs))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(def ^:private raw-ns #{:prev})
(defn- ns-dir [ns] (str "trace-" (name ns)))

;; --- live ---------------------------------------------------------------------------

(defn- gitdir [repo & more] (str (apply fs/path repo ".git" more)))

(defmacro ^:private in [repo & body] `(binding [g/*repo* ~repo] ~@body))

(defn- live-tip [ref]
  (let [r (g/git* {} "rev-parse" "--verify" "-q" ref)]
    (when (zero? (:exit r)) (str/trim (:out r)))))

(defn- live-blob [content]
  (git "hash-object" "-w" "--stdin"
       {:in (if (bytes? content) (ByteArrayInputStream. content) (str content))}))

(defn- live-add-note [repo ref sha content]
  (let [env {:extra-env {"GIT_INDEX_FILE" (gitdir repo "trace-index")}}
        parent (live-tip ref)]
    (if parent (git "read-tree" parent env) (git "read-tree" "--empty" env))
    (git "update-index" "--add" "--cacheinfo" (str "100644," (live-blob content) "," sha) env)
    (git "update-ref" ref
         (apply git "commit-tree" (git "write-tree" env)
                (concat (when parent ["-p" parent]) ["-m" (last (str/split ref #"/"))])))))

(defn- kv-file [repo ns k] (gitdir repo (ns-dir ns) (clean-key k)))

(defn- read-kv [ns f]
  (when (fs/exists? f) (if (raw-ns ns) (slurp f) (edn/read-string (slurp f)))))

(defrecord Live [repo]
  Store
  (ensure! [_]
    (when-not (fs/exists? (gitdir repo))
      (fs/create-dirs repo)
      (in repo (git "init" "-q" "-b" "_" {:dir repo}))
      (fs/create-dirs (gitdir repo "trace-sessions"))))
  (locked [_ f]
    (let [lock (gitdir repo "trace.lock")]
      (loop [n 0]
        (if (try (fs/create-dir lock) true (catch Exception _ false))
          (try (f) (finally (fs/delete-tree lock)))
          (if (< n 300)
            (do (Thread/sleep 50) (recur (inc n)))
            ;; a lock this old is a crashed hook's; take it over
            (do (fs/delete-tree lock) (recur 0)))))))
  (tip [_ ref] (in repo (live-tip ref)))
  (refs [_ prefix]
    (in repo (remove str/blank? (str/split-lines (git "for-each-ref" "--format=%(refname)" (str prefix "*"))))))
  (has? [_ rev path] (in repo (zero? (:exit (g/git* {} "cat-file" "-e" (str rev ":" path))))))
  (blob! [_ content] (in repo (live-blob content)))
  (commit! [_ {:keys [parents base files subject body]}]
    (in repo
      (let [env {:extra-env {"GIT_INDEX_FILE" (gitdir repo "trace-index")}}]
        (if base (git "read-tree" base env) (git "read-tree" "--empty" env))
        (doseq [[path content] files]
          (git "update-index" "--add" "--cacheinfo" (str "100644," (live-blob content) "," path) env))
        (apply git "commit-tree" (git "write-tree" env)
               (concat (mapcat #(vector "-p" %) parents)
                       ["-m" subject] (when-not (str/blank? body) ["-m" body]))))))
  (advance! [_ ref id] (in repo (git "update-ref" ref id)))
  (note! [_ ref id content] (in repo (live-add-note repo ref id content)))
  (kv [_ ns k] (read-kv ns (kv-file repo ns k)))
  (kv! [_ ns k v]
    (let [f (kv-file repo ns k)]
      (if (nil? v)
        (fs/delete-if-exists f)
        (do (fs/create-dirs (fs/parent f)) (spit f (if (raw-ns ns) v (pr-str v)))))))
  (kv-all [_ ns]
    (let [d (gitdir repo (ns-dir ns))]
      (if (fs/exists? d)
        (into {} (keep #(when-let [v (try (read-kv ns (str %)) (catch Exception _ nil))] [(str (fs/file-name %)) v]))
              (fs/list-dir d))
        {}))))

(defn live
  "The live store of a trace repo."
  [repo] (->Live (str repo)))

;; --- batch --------------------------------------------------------------------------

(def ^:private ident "reus-trace <trace@reus.local>")

(defn git-date
  "git's raw date for epoch milliseconds, in the local zone."
  [ms]
  (let [i (Instant/ofEpochMilli ms)
        off (.getTotalSeconds (.getOffset (.getRules (ZoneId/systemDefault)) i))]
    (format "%d %s%02d%02d" (quot ms 1000) (if (neg? off) "-" "+") (quot (abs off) 3600) (quot (mod (abs off) 3600) 60))))

(defn- w [^OutputStream o s] (.write o (bytes-of s)))

(defn- data [^OutputStream o content]
  (let [bs (bytes-of content)]
    (w o (str "data " (alength bs) "\n"))
    (.write o bs)
    (w o "\n")))

(defn- quote-path [p]
  (if (re-find #"[\n\"\\]|^\"" p)
    (str "\"" (-> p (str/replace "\\" "\\\\") (str/replace "\"" "\\\"") (str/replace "\n" "\\n")) "\"")
    p))

(defn- valid-path?
  "Would git take path into a tree? (update-index refuses a .git component, which the live
  store then silently leaves out of the commit.)"
  [path]
  (every? #(not (or (#{"" "." ".."} %) (= ".git" (str/lower-case (str/replace % #"[ .]+$" "")))))
          (str/split path #"/" -1)))

(defn- emit-blob!
  "The mark of content in the stream, written when it is new."
  [{:keys [out st]} content]
  (let [sha (blob-sha content)]
    (or (get-in @st [:blobs sha])
        (let [m (str ":" (:n (swap! st update :n inc)))]
          (w out (str "blob\nmark " m "\n"))
          (data out content)
          (swap! st assoc-in [:blobs sha] m)
          m))))

(defn- emit-commit! [{:keys [out st]} ref msg parents tree-lines]
  (let [m (str ":" (:n (swap! st update :n inc)))
        d (:date @st)]
    (w out (str (when (empty? parents) (str "reset " ref "\n")) "commit " ref "\nmark " m "\n"
                "author " ident " " d "\ncommitter " ident " " d "\n"))
    (data out msg)
    (when (seq parents) (w out (str "from " (first parents) "\n")))
    (doseq [p (rest parents)] (w out (str "merge " p "\n")))
    (doseq [l tree-lines] (w out (str l "\n")))
    (w out "\n")
    m))

(defrecord Batch [repo proc out st]
  Store
  (ensure! [_])
  (locked [_ f] (f))
  (tip [_ ref] (get-in @st [:refs ref]))
  (refs [_ prefix] (filter #(str/starts-with? % prefix) (keys (:refs @st))))
  (has? [_ rev path] (contains? (get-in @st [:trees rev]) path))
  (blob! [this content] (emit-blob! this content) (blob-sha content))
  (commit! [this {:keys [parents base files subject body]}]
    (let [tree (get-in @st [:trees base] {})
          added (into {} (keep (fn [[path content]] (when (valid-path? path) [path (emit-blob! this content)]))) files)
          tree' (merge tree added)
          same? (and base (= base (first parents)))
          m-line (fn [[path mk]] (str "M 100644 " mk " " (quote-path path)))
          lines (if same? (map m-line added) (cons "deleteall" (map m-line tree')))
          complete (fn [t] (if (str/ends-with? t "\n") t (str t "\n")))
          msg (str (complete (str subject)) (when-not (str/blank? body) (str "\n" (complete body))))
          id (emit-commit! this "refs/tmp/fi" msg parents lines)]
      (swap! st assoc-in [:trees id] tree')
      id))
  (advance! [_ ref id] (swap! st assoc-in [:refs ref] id) nil)
  (note! [this ref id content]
    (emit-blob! this content)
    (swap! st assoc-in [:notes ref id] (blob-sha content))
    nil)
  (kv [_ ns k] (get-in @st [:kv ns (clean-key k)]))
  (kv! [_ ns k v]
    (swap! st (fn [s] (if (nil? v) (update-in s [:kv ns] dissoc (clean-key k)) (assoc-in s [:kv ns (clean-key k)] v))))
    nil)
  (kv-all [_ ns] (get-in @st [:kv ns] {})))

(defn batch
  "A replay's store for the (new) trace repo; a git fast-import is running until (close! s)."
  [repo]
  (let [repo (str repo)]
    (fs/create-dirs repo)
    (g/git* {:dir repo} "init" "-q" "-b" "_")
    (let [pb (doto (ProcessBuilder. ["git" "-c" "user.name=reus-trace" "-c" "user.email=trace@reus.local"
                                     "fast-import" "--quiet" "--force" "--date-format=raw" "--done"])
               (.directory (fs/file repo))
               (.redirectOutput java.lang.ProcessBuilder$Redirect/INHERIT)
               (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
          proc (.start pb)]
      (->Batch repo proc (BufferedOutputStream. (.getOutputStream proc) 65536)
               (atom {:n 0 :refs {} :trees {} :blobs {} :notes {} :kv {} :date (git-date (System/currentTimeMillis))})))))

(defn at! "Date what the batch store writes next (epoch ms)." [s ms]
  (swap! (:st s) assoc :date (git-date ms)) nil)

(defn close!
  "Write the refs and notes, finish fast-import. Throws when it failed."
  [{:keys [repo proc out st] :as s}]
  (let [{:keys [refs notes]} @st]
    (doseq [[ref id] refs] (w out (str "reset " ref "\nfrom " id "\n\n")))
    (doseq [[ref m] notes]
      (emit-commit! s ref (str (last (str/split ref #"/")) "\n") []
                    (map (fn [[id sha]] (str "N " (get-in @st [:blobs sha]) " " id)) m)))
    (w out "done\n")
    (.close out)
    (let [code (.waitFor proc)]
      (g/git* {:dir repo} "update-ref" "-d" "refs/tmp/fi")
      (when-not (zero? code) (throw (ex-info "git fast-import failed" {:exit code}))))
    nil))
