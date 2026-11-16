(ns trace.copilot-replay
  "Old Copilot sessions into a trace repo, files and all (the CLI and the IntelliJ plugin write the same log):

    bb copilot:replay [SESSION-ID | events.jsonl | session dir]... [--home DIR] [--repo DIR] [--map ORIG=DIR]... [--force]

  No session named: every session of the Copilot home (--home, else $COPILOT_HOME, else ~/.copilot). The repo
  (default var/copilot-replay; never the live one) is refused when it exists, unless --force replaces it.

  The events are the hook payloads trace.copilot-events reads off <home>/session-state/<id>/events.jsonl, adapted
  by trace.copilot as the hooks would have been (model, tokens and time per call as live), every selected
  session in one stream sorted by time, through trace.replay's engine (run-events!: a batch store and a shadow
  of each project as it was then). Unlike bb copilot:catchup the trees are the files as they were at each call:
    HEAD           the commit the worktree had then (the repo's reflog and history, trace.shadow)
    a write        a successful edit (old_str -> new_str), create (file_text), str_replace_editor (create,
                   str_replace, insert) or apply_patch (the patch text, see apply-patch) on the file as the
                   shadow has it when the call starts
    a preimage     Copilot's rewind snapshots (session-state/<id>/rewind-file-snapshots/index.json): per prompt,
                   every file the prompt changed as it was before (a backup, or absent), put in before the
                   prompt's first call on it
  What nothing proves (a file a shell command wrote, a file of a repo that is gone) is as the shadow has it.

  The check: a snapshot also has each file's sha256 after the prompt (its postimage). At the end of the prompt
  the replayed file is hashed and compared: :post {:ok n :bad n :bad-files [...]} in the stats."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [trace.copilot :as cop]
            [trace.copilot-events :as ce]
            [trace.git :as g]
            [trace.replay :as rp]
            [trace.shadow :as shadow])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(def default-repo (str (fs/path (fs/parent g/default-repo) "copilot-replay")))

(defn- ms [ts] (try (.toEpochMilli (Instant/parse (str ts))) (catch Exception _ nil)))
(defn- stat! [st k] (swap! st update k (fnil inc 0)))

(defn sha256 [^String s]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(defn workspace
  "{key value} of the workspace.yaml in dir (flat `key: value` lines), {} without one."
  [dir]
  (let [f (fs/path dir "workspace.yaml")]
    (if (fs/regular-file? f)
      (into {} (keep #(when-let [[_ k v] (re-matches #"([A-Za-z_]+):\s*(.*)" %)] [k (str/trim v)])
                     (str/split-lines (slurp (str f)))))
      {})))

;; --- apply_patch ------------------------------------------------------------------------

(defn- resolve-path [cwd p]
  (let [p (str/trim (str p))] (str (fs/normalize (if (or (fs/absolute? p) (nil? cwd)) p (fs/path cwd p))))))

(defn parse-patch
  "The files of an apply_patch text (`*** Begin Patch` ... `*** End Patch`):
  [{:op :add|:update|:delete :path :to (a move) :lines [+line] (add) :chunks [{:ctx header :old [..] :new [..] :eof? bool}]}]."
  [text]
  (let [ls (str/split-lines (str/replace (str text) "\r\n" "\n"))
        start (fn [l] (when-let [[_ op p] (re-matches #"\*\*\* (Add|Update|Delete) File: (.+?)\s*" l)]
                        {:op (keyword (str/lower-case op)) :path p :lines [] :chunks []}))
        add-line (fn [f l]
                   (let [c (first l) body (subs l (min 1 (count l)))]
                     (case (:op f)
                       :add (update f :lines conj body)
                       :update (let [f (if (empty? (:chunks f)) (update f :chunks conj {:old [] :new []}) f)
                                     i (dec (count (:chunks f)))]
                                 (case c
                                   \+ (update-in f [:chunks i :new] conj body)
                                   \- (update-in f [:chunks i :old] conj body)
                                   ;; a context line (a space, or an empty line some models write for one)
                                   (-> f (update-in [:chunks i :old] conj body) (update-in [:chunks i :new] conj body))))
                       f)))]
    (loop [[l & more :as left] ls cur nil out []]
      (let [done (fn [] (if cur (conj out cur) out))]
        (cond
          (empty? left) (done)
          (start l) (recur more (start l) (done))
          (nil? cur) (recur more nil out)
          (re-matches #"\*\*\* End Patch\s*" l) (recur more nil (done))
          (re-matches #"\*\*\* Begin Patch\s*" l) (recur more cur out)
          (re-matches #"\*\*\* Move to: (.+?)\s*" l) (recur more (assoc cur :to (second (re-matches #"\*\*\* Move to: (.+?)\s*" l))) out)
          (re-matches #"\*\*\* End of File\s*" l) (recur more (cond-> cur (seq (:chunks cur)) (assoc-in [:chunks (dec (count (:chunks cur))) :eof?] true)) out)
          (str/starts-with? l "@@") (recur more (update cur :chunks conj {:ctx (str/trim (subs l 2)) :old [] :new []}) out)
          :else (recur more (add-line cur l) out))))))

(defn- find-seq
  "Index >= from where lines `want` start in ls, matched exactly, else ignoring trailing space, else ignoring
  space at both ends (as Codex's apply_patch does), or nil. eof?: tried at the end first."
  [ls want from eof?]
  (let [n (count want)
        at? (fn [norm i] (every? true? (map #(= (norm %1) (norm %2)) (subvec ls i (+ i n)) want)))
        idxs (range from (inc (- (count ls) n)))]
    (some (fn [norm] (or (when (and eof? (<= from (- (count ls) n))) (let [i (- (count ls) n)] (when (at? norm i) i)))
                         (some #(when (at? norm %) %) idxs)))
          [identity str/trimr str/trim])))

(defn- update-text
  "old (a file's text) with the chunks of an Update File applied: [new-text misses]."
  [old chunks]
  (let [ls (vec (let [v (str/split-lines old)] (if (and (seq v) (= "" (peek v))) (pop v) v)))
        [ls _ misses]
        (reduce (fn [[ls from misses] {:keys [ctx old new eof?]}]
                  (let [from (if (str/blank? ctx) from (or (some-> (find-seq ls [ctx] from false) inc) from))]
                    (if (empty? old)
                      [(into ls new) (+ (count ls) (count new)) misses] ; pure addition: at the end
                      (if-let [i (find-seq ls old from eof?)]
                        [(vec (concat (subvec ls 0 i) new (subvec ls (+ i (count old))))) (+ i (count new)) misses]
                        [ls from (inc misses)]))))
                [ls 0 0] chunks)]
    [(str (str/join "\n" ls) (when (seq ls) "\n")) misses]))

(defn apply-patch
  "What an apply_patch text does to the files: [[path content-or-nil] ...] (nil: the file is gone) and the
  number of chunks whose old lines were not found (those chunks are left out). content-of (fn [path]) is a
  file's text before; relative paths are resolved against cwd."
  [text cwd content-of]
  (reduce (fn [[out misses] {:keys [op path to lines chunks]}]
            (let [p (resolve-path cwd path)]
              (case op
                :add [(conj out [p (str (str/join "\n" lines) (when (seq lines) "\n"))]) misses]
                :delete [(conj out [p nil]) misses]
                :update (let [[t m] (update-text (or (content-of p) "") chunks)]
                          [(if to (conj out [p nil] [(resolve-path cwd to) t]) (conj out [p t])) (+ misses m)]))))
          [[] 0] (parse-patch text)))

;; --- what a call proves -------------------------------------------------------------------

(defn- patch-text [args] (if (string? args) args (or (get args "input") (get args "patch"))))

;; --- here-documents ----------------------------------------------------------------------

(defn- unquote* [p] (if (re-matches #"(['\"]).*\1" (str p)) (subs p 1 (dec (count p))) p))

(def ^:private heredoc-res
  "Lines that send a here-document into a file, [regex kind]; hit-of names the groups of each kind."
  (let [path "([^\\s;&|<>'\"]+|'[^']+'|\"[^\"$`]+\")" doc "<<(-?)\\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\\4"]
    [[(re-pattern (str "\\bcat\\s*(>>?)\\s*" path "\\s*" doc)) :cat]
     [(re-pattern (str "\\bcat\\s*<<(-?)\\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\\2\\s*(>>?)\\s*" path)) :cat-after]
     [(re-pattern (str "\\btee\\s+(-a\\s+)?" path "\\s*" doc)) :tee]]))

(defn- hit-of [kind [_ g1 g2 g3 g4 g5]]
  (let [[append? path dash quote word] (case kind
                                         :cat [(= ">>" g1) g2 g3 g4 g5]
                                         :cat-after [(= ">>" g4) g5 g1 g2 g3]
                                         :tee [(some? g1) g2 g3 g4 g5])]
    {:append? append? :path (unquote* path) :dash? (= "-" dash) :quoted? (not (str/blank? quote)) :word word}))

(defn heredoc-writes
  "The files a shell command writes from a here-document (`cat > f <<'EOF'`, `cat >> f`, `cat <<EOF > f`,
  `tee [-a] f <<EOF`), in order: [[path text append?] ...]. Paths are resolved against a leading `cd DIR &&`, else
  cwd. A document whose word is not quoted and whose text has `$`, a backquote or a backslash is left out (the
  shell would have changed it), as is a path with `$`, `~` or a glob in it."
  [cmd cwd]
  (let [ls (str/split-lines (str cmd))
        base (if-let [[_ d] (re-find #"^\s*cd\s+([^\s;&|]+)\s*(?:&&|;)" (str (first ls)))] (resolve-path cwd (unquote* d)) cwd)]
    (loop [i 0 out []]
      (if (>= i (count ls))
        out
        (let [l (nth ls i)
              hit (some (fn [[re kind]] (some->> (re-find re l) (hit-of kind))) heredoc-res)]
          (if-not hit
            (recur (inc i) out)
            (let [{:keys [path dash? quoted? word append?]} hit
                  body (take-while #(not= word (if dash? (str/replace % #"^\t+" "") %)) (drop (inc i) ls))
                  body (if dash? (map #(str/replace % #"^\t+" "") body) body)
                  text (str (str/join "\n" body) "\n")
                  ok? (and (or quoted? (not (re-find #"[$`\\]" text))) (not (re-find #"[$~*?\[]" path)))]
              (recur (+ i 2 (count body))
                     (cond-> out ok? (conj [(resolve-path base path) text append?]))))))))))

(defn- heredoc-files [cmd cwd] (distinct (map first (heredoc-writes cmd cwd))))

(defn- touched
  "The paths a Copilot call writes (its tool name and arguments), resolved against cwd."
  [cwd tool args]
  (case tool
    ("edit" "create" "str_replace_editor") (when-let [p (get args "path")] [(resolve-path cwd p)])
    "apply_patch" (mapcat (fn [{:keys [path to]}] (cond-> [(resolve-path cwd path)] to (conj (resolve-path cwd to))))
                          (parse-patch (patch-text args)))
    ("bash" "powershell") (heredoc-files (get args "command") cwd)
    nil))

(defn view-text
  "The whole file a `view` call showed, when it showed all of it: no view_range, and Copilot's own diff of the
  view (detailedContent) is one hunk @@ -1,N +1,N @@ over N lines of content. Else nil."
  [args result]
  (let [c (get result "content") d (str (get result "detailedContent"))]
    (when (and (string? c) (nil? (get args "view_range")) (not (str/includes? d "create file mode")))
      (when-let [[_ a b] (re-find #"(?m)^@@ -1,(\d+) \+1,(\d+) @@" d)]
        (when (= a b (str (count (str/split c #"\n" -1)))) c)))))

(defn written
  "[[path content-or-nil] ...] a successful Copilot call wrote, and the misses: [writes misses]. content-of
  (fn [path]) is a file's text before the call."
  [cwd tool args content-of]
  (let [p (some->> (get args "path") (resolve-path cwd))
        repl (fn [s o n] (when (and s o n (str/includes? s o)) (str/replace-first s o n)))]
    (case tool
      "edit" (if-let [t (repl (content-of p) (get args "old_str") (get args "new_str"))] [[[p t]] 0] [[] 1])
      "create" (if-let [t (get args "file_text")] [[[p t]] 0] [[] 0])
      "str_replace_editor"
      (case (get args "command")
        "create" [[[p (str (get args "file_text"))]] 0]
        "str_replace" (if-let [t (repl (content-of p) (get args "old_str") (get args "new_str"))] [[[p t]] 0] [[] 1])
        "insert" (if-let [s (content-of p)]
                   (let [ls (str/split s #"\n" -1) n (min (count ls) (max 0 (or (get args "insert_line") 0)))
                         new (str/split (str (get args "new_str")) #"\n" -1)]
                     [[[p (str/join "\n" (concat (take n ls) new (drop n ls)))]] 0])
                   [[] 1])
        [[] 0])
      "apply_patch" (if-let [t (patch-text args)] (apply-patch t cwd content-of) [[] 0])
      ("bash" "powershell")
      [(second (reduce (fn [[now out] [p text append?]]
                         (let [c (if append? (str (get now p (content-of p)) text) text)]
                           [(assoc now p c) (conj out [p c])]))
                       [{} []] (heredoc-writes (get args "command") cwd)))
       0]
      [[] 0])))

(defn- text-of
  "The text of a file whose bytes are UTF-8 text, else nil."
  [f]
  (when (fs/regular-file? f)
    (let [bs (fs/read-all-bytes f) c (String. bs "UTF-8")]
      (when (java.util.Arrays/equals ^bytes (.getBytes c "UTF-8") ^bytes bs) c))))

(defn- post-ok? [post have] (if (= :absent post) (nil? have) (and (some? have) (= post (sha256 have)))))

(defn- fill!
  "A file at the end of a prompt, as Copilot hashed it (post): when the shadow does not have that, the content
  found under that hash (a backup, today's disk), or gone for :absent. Counted in st."
  [sh st {:keys [path post content]}]
  (let [have (shadow/content sh path)]
    (cond (post-ok? post have) nil
          (= :absent post) (do (shadow/delete! sh path) (stat! st :filled))
          content (do (shadow/write! sh path content) (stat! st :filled))
          :else (stat! st :unknown))))

(defn- evidence!
  "Into the shadow, what a successful call wrote, then the files of its prompt it is the last call to touch
  or name (fill!). Counted in st."
  [sh st {:keys [payload copilot fills]}]
  (let [{:keys [tool args cwd result]} copilot
        ok? (not= "PostToolUseFailure" (get payload "hook_event_name"))]
    ;; a failed shell command may well have written its here-documents before it failed; a failed edit wrote nothing
    (when (or ok? (#{"bash" "powershell"} tool))
      (let [[writes misses] (written cwd tool args #(shadow/content sh %))]
        (doseq [[p c] writes]
          (if (nil? c) (shadow/delete! sh p) (shadow/write! sh p c))
          (stat! st (keyword (str "write-" tool))))
        (when (pos? misses)
          (swap! st update :write-misses (fnil + 0) misses)
          (swap! st update :missed (fnil conj []) [(:file-name copilot) (:line copilot) tool (some-> (get args "path") str)]))))
    ;; a view of a whole file the shadow has otherwise: what nothing else showed changed it
    (when (and ok? (= "view" tool))
      (let [p (some->> (get args "path") (resolve-path cwd)) have (some->> p (shadow/content sh)) c (view-text args result)]
        (when (and have c (not= have c))
          (shadow/write! sh p c)
          (stat! st :view-fix)))))
  (doseq [fl fills] (fill! sh st fl)))

(defn- side!
  "A preimage goes into the shadow; a fill puts in a file's end of prompt no call explains; a check compares the
  replayed file with Copilot's hash of it."
  [sh st {:keys [kind ts path backup absent? post session branch] :as ev}]
  (shadow/sync! sh path ts branch)
  (case kind
    :pre (let [have (shadow/content sh path)]
           (stat! st :pre)
           (cond
             absent? (when have (shadow/delete! sh path) (stat! st :pre-applied))
             :else (if-let [c (some-> backup text-of)]
                     (when (not= c have) (shadow/write! sh path c) (stat! st :pre-applied))
                     (stat! st :pre-missing))))
    :fill (fill! sh st ev)
    :check (let [ok? (post-ok? post (shadow/content sh path))]
             (swap! st update-in [:post (if ok? :ok :bad)] (fnil inc 0))
             (when-not ok? (swap! st update-in [:post :bad-files] (fnil conj []) [session path])))
    nil))

;; --- the events of a log ------------------------------------------------------------------

(defn- snapshots
  "Copilot's rewind snapshots of a session dir: [{:event-id :ts :files [{:path :backup :absent? :post}]}]."
  [dir]
  (let [f (fs/path dir "rewind-file-snapshots" "index.json")]
    (when (fs/regular-file? f)
      (for [{:strs [eventId timestamp files]} (get (json/parse-string (slurp (str f))) "snapshots")]
        {:event-id eventId :ts (ms timestamp)
         :files (for [[_ {:strs [path preimage postimage]}] files :when path]
                  {:path path
                   :absent? (not= "content" (get preimage "kind"))
                   :backup (some->> (get preimage "backupFile") (fs/path dir "rewind-file-snapshots" "backups") str)
                   :post (if (= "content" (get postimage "kind")) (get postimage "contentHash") :absent)})}))))

(defn- by-hash
  "The text of a file with sha256 h: a rewind backup of any session in the Copilot home (backups are named
  by their hash), else the file at path today when it has that hash. nil when none has."
  [state-dir h path]
  (or (some text-of (fs/glob state-dir (str "*/rewind-file-snapshots/backups/" h)))
      (let [c (text-of path)] (when (and c (= h (sha256 c))) c))))

(defn- names?
  "Does shell command cmd name the file path (relative to cwd, or by its name as a word)?"
  [cmd cwd path]
  (let [cmd (str cmd)
        rel (when (and cwd (str/starts-with? path (str cwd "/"))) (subs path (inc (count cwd))))
        n (str (fs/file-name path))]
    (boolean (or (str/includes? cmd path)
                 (and rel (str/includes? cmd rel))
                 (re-find (re-pattern (str "(?<![\\w.-])" (java.util.regex.Pattern/quote n) "(?![\\w-])")) cmd)))))

(def ^:private order {:pre -1 :prompt 0 :subagent-start 1 :tool 2 :subagent-stop 3 :fill 3.5 :stop 4 :check 5})

(defn session-events
  "The replay events of one Copilot log f, unsorted: what the hooks would have been given (trace.copilot-events,
  trace.copilot); per rewind snapshot, its files as they were when the prompt came (:pre) and as they were at its
  end (the content found under the postimage hash, put in after the prompt's last call that touches or names the
  file, else just before the answer: :fills of a tool event, :fill) and the check (:check)."
  [f]
  (let [f (str (fs/canonicalize f))
        dir (fs/parent f)
        all (ce/read-log f)
        ws (workspace dir)
        sid (get-in (first all) ["data" "sessionId"])
        cwd (get-in (first all) ["data" "context" "cwd"])
        branch (get ws "branch")
        tstart (into {} (keep #(when (= "tool.execution_start" (get % "type"))
                                 [(get-in % ["data" "toolCallId"]) (ms (get % "timestamp"))]) all))
        hook-evs
        (vec (for [{:keys [i payload es]} (ce/payloads all f)
                   :let [subagent? (= "SubagentStart" (get payload "hook_event_name"))
                         [event input] (cop/adapt (assoc payload "transcript_path" f) {:es (if subagent? all es)})
                         e (get all i)
                         tool? (= "tool" event)]
                   :when event]
               (cond-> {:kind (keyword event) :ts (ms (get e "timestamp")) :file f :line (inc i) :i i
                        ;; a relative file (apply_patch names them so) would leave the call in no worktree
                        :payload (cond-> (assoc input "replay" {"file" f "line" (inc i)})
                                   (some-> (get-in input ["tool_input" "file_path"]) fs/relative?)
                                   (update-in ["tool_input" "file_path"] #(resolve-path (get payload "cwd") %)))
                        :branch branch}
                 tool? (assoc :start (get tstart (get-in e ["data" "toolCallId"]))
                              :copilot {:tool (get payload "tool_name") :args (get payload "tool_input") :cwd (get payload "cwd")
                                        :result (get-in e ["data" "result"]) :file-name f :line (inc i)}))))
        ;; a prompt's range of the log: from its user.message to the next one of the main agent
        users (keep-indexed (fn [i e] (when (and (= "user.message" (get e "type")) (nil? (get e "agentId"))) [i (get e "id")])) all)
        ranges (into {} (map (fn [[i id] [j _]] [id [i (or j (count all))]]) users (concat (rest users) [nil])))
        state-dir (fs/parent dir)
        plan (for [{:keys [event-id ts files]} (snapshots dir)
                   :let [[i j] (get ranges event-id)]
                   :when i
                   ;; the prompt's hook comes a moment before its user.message: the files are put in before both
                   :let [turn-start (apply min (ms (get-in all [i "timestamp"]))
                                           (keep #(when (and (= :prompt (:kind %)) (< (- i 10) (:i %) i)) (:ts %)) hook-evs))
                         turn-end (or (some #(ms (get % "timestamp")) (rseq (subvec all i j))) ts)
                         in-turn (filter #(< i (:i %) j) hook-evs)
                         stop (first (filter #(= :stop (:kind %)) in-turn))
                         calls (filter #(= :tool (:kind %)) in-turn)]
                   {:keys [path backup absent? post]} files
                   :let [last-call (last (filter (fn [{:keys [copilot payload]}]
                                                   (or (some #{path} (touched (:cwd copilot) (:tool copilot) (:args copilot)))
                                                       (and (= "bash" (:tool copilot)) (names? (get-in payload ["tool_input" "command"]) cwd path))))
                                                 (sort-by (juxt :ts :line) calls)))
                         fl {:path path :post post :content (when (string? post) (by-hash state-dir post path))}]]
               {:pre {:kind :pre :ts turn-start :file f :line (inc i) :path path :backup backup :absent? absent? :session sid :branch branch}
                :fill (if last-call
                        [(:line last-call) fl]
                        (merge fl {:kind :fill :ts (or (:ts stop) turn-end) :file f :line (or (:line stop) j) :session sid :branch branch}))
                :check {:kind :check :ts (min ts turn-end) :file f :line j :path path :post post :session sid :branch branch}})
        fills (reduce (fn [m [line fl]] (update m line (fnil conj []) fl)) {} (filter vector? (map :fill plan)))]
    (concat (map #(if-let [fl (fills (:line %))] (assoc % :fills fl) %) hook-evs)
            (map :pre plan) (remove vector? (map :fill plan)) (map :check plan))))

(defn events
  "Every event of the given logs, in time order."
  [files]
  (->> files
       (mapcat session-events)
       (filter :ts)
       (sort-by (juxt :ts #(order (:kind %)) :file :line))))

(defn replay!
  "Replay Copilot logs files into repo (a new one); maps {orig-root repo-dir}. Returns stats."
  [files repo maps]
  (rp/run-events! (events files) repo maps {:evidence evidence! :side side!}))

;; --- command line --------------------------------------------------------------------------

(defn logs
  "The events.jsonl files sel names in Copilot home: a file, a session dir, a session id (a prefix will do);
  every session of the home for none."
  [home sel]
  (let [state (fs/path home "session-state")
        all #(sort (map str (fs/glob state "*/events.jsonl")))]
    (if (empty? sel)
      (all)
      (distinct
       (mapcat (fn [s]
                 (cond (fs/regular-file? s) [(str (fs/canonicalize s))]
                       (fs/regular-file? (fs/path s "events.jsonl")) [(str (fs/canonicalize (fs/path s "events.jsonl")))]
                       :else (filter #(str/starts-with? (str (fs/file-name (fs/parent %))) s) (all))))
               sel)))))

(defn -main [& argv]
  (let [{:keys [sel repo maps force home]}
        (loop [[a b & more :as as] argv o {:maps {}}]
          (cond (empty? as) o
                (= a "--repo") (recur more (assoc o :repo b))
                (= a "--home") (recur more (assoc o :home b))
                (= a "--map") (recur more (update o :maps conj (str/split b #"=" 2)))
                (= a "--force") (recur (rest as) (assoc o :force true))
                :else (recur (rest as) (update o :sel (fnil conj []) a))))
        home (or home (cop/copilot-home))
        repo (str (fs/absolutize (or repo default-repo)))
        files (logs home sel)]
    (cond
      (empty? files) (do (println "usage: bb copilot:replay [SESSION-ID | events.jsonl | dir]... [--home DIR] [--repo DIR] [--map ORIG=DIR]... [--force]"
                                  (str "\nno Copilot session logs" (when sel (str " for " (str/join " " sel))) " in " home))
                         (System/exit 1))
      (some #{repo} (map #(str (fs/absolutize %)) [g/default-repo g/*repo*]))
      (do (println "refusing to replay into the live trace repo" repo) (System/exit 1))
      (and (fs/exists? repo) (not force))
      (do (println repo "exists; --force replaces it") (System/exit 1))
      :else
      (do (when (fs/exists? repo) (fs/delete-tree repo))
          (println "replaying" (count files) "Copilot sessions into" repo)
          (let [s (replay! files repo maps)
                {:keys [ok bad bad-files]} (:post s)]
            (println (pr-str (-> s (dissoc :post) (update :missed #(take 20 %)))))
            (println (str "files checked against Copilot's own hash after each prompt: " (or ok 0) " right, " (or bad 0) " wrong"))
            (doseq [[sid p] (take 20 bad-files)] (println "  wrong:" (subs (str sid "--------") 0 8) p))
            (println "look at it: bb trace:gui" repo))))))
