(ns trace.replay
  "Old sessions into a trace repo: the hook payloads are rebuilt from the transcripts
  and fed, in time order, to trace.record, which runs against a shadow of each project
  as it was then (trace.shadow), with git dates set to the event times.

    bb trace:replay <sel> [--repo dir] [--map orig=dir]... [--force]
  sel as in jsonl:ls; every selected session, with its subagent runs, is one stream
  sorted by time, so prompt numbers follow reality across concurrent sessions. The repo
  (default var/replay; never the live one) is refused when it exists, unless --force
  replaces it; the shadows are <repo>-wt.

  From a transcript:
    prompt          a typed prompt; a slash command as `/name args`; a
                    <task-notification> that opens a turn (queue `remove`, or a user
                    entry), once per task id. Compaction summaries and meta messages are not.
    tool            a tool_use with its tool_result, at the result's time; tool_response is
                    the entry's toolUseResult; is_error -> PostToolUseFailure with `error`
    stop            at the turn's stop-hook summary (else its last assistant entry), with
                    the turn's last text; SessionEnd at the end of the file
    subagent-*      an agent file's first / last entry; its calls carry agent_id/agent_type
  Each payload has `replay` {file line}, kept in its note.

  Before record sees a tool event, the shadow gets what the transcript proves: an
  Edit/MultiEdit/Write's resulting file (from originalFile and the input), a Bash call's
  bashEditDiff hunks, a full Read's content. Worktrees are the ones a transcript adds
  (`git worktree add`) plus the repo's current ones."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [jsonl.read :as jr]
            [trace.git :as g]
            [trace.record :as rec]
            [trace.shadow :as shadow])
  (:import [java.time Instant ZoneId]))

(def default-repo (str (fs/path (fs/parent g/default-repo) "replay")))

(defn- ms [ts] (try (.toEpochMilli (Instant/parse ts)) (catch Exception _ nil)))

(defn- lines
  "[line-no entry] of a transcript, 1-based, unparsable lines skipped."
  [f]
  (with-open [r (io/reader (str f))]
    (into [] (keep-indexed (fn [i l] (when-let [e (try (json/parse-string l) (catch Exception _ nil))] [(inc i) e])))
          (line-seq r))))

(defn- blocks [e] (let [c (get-in e ["message" "content"])] (if (sequential? c) c [])))
(defn- texts [e] (str/join "\n" (keep #(when (= "text" (get % "type")) (get % "text")) (blocks e))))
(defn- result-text [c] (if (string? c) c (str/join "\n" (keep #(get % "text") c))))
(defn- tag [s t] (some-> (re-find (re-pattern (str "(?s)<" t ">(.*?)</" t ">")) (str s)) second str/trim))

(defn- prompt-of
  "The prompt a user/queue entry submitted, or nil."
  [e]
  (let [c (get-in e ["message" "content"])]
    (cond
      (= "queue-operation" (get e "type"))
      (when (and (= "remove" (get e "operation")) (str/starts-with? (str (get e "content")) "<task-notification>"))
        (get e "content"))
      (not= "user" (get e "type")) nil
      (get e "isMeta") nil
      (and (string? c) (str/starts-with? c "<command-name>"))
      (str/trim (str (tag c "command-name") " " (tag c "command-args")))
      (and (string? c) (str/starts-with? c "<task-notification>")) c
      :else (jr/prompt-text (json/parse-string (json/generate-string e) true)))))

(defn- event [kind ts file line payload & [extra]]
  (merge {:kind kind :ts ts :file (str file) :line line
          :payload (assoc payload "replay" {"file" (str file) "line" line})}
         extra))

(defn- tool-events
  "Tool events of a transcript's entries; agent: {agent_id agent_type} or nil."
  [f es sid agent]
  (let [uses (into {} (for [[_ e] es b (blocks e) :when (= "tool_use" (get b "type"))] [(get b "id") b]))]
    (for [[n e] es :when (= "user" (get e "type"))
          b (blocks e) :when (= "tool_result" (get b "type"))
          :let [u (get uses (get b "tool_use_id"))] :when u]
      (let [err? (true? (get b "is_error"))
            r (get e "toolUseResult")]
        (event :tool (ms (get e "timestamp")) f n
               (cond-> (merge {"session_id" sid "cwd" (get e "cwd")
                               "hook_event_name" (if err? "PostToolUseFailure" "PostToolUse")
                               "tool_name" (get u "name") "tool_input" (get u "input")
                               "tool_use_id" (get u "id")
                               "tool_response" (if (some? r) r (get b "content"))}
                              agent)
                 err? (assoc "error" (result-text (get b "content"))))
               {:branch (get e "gitBranch") :result r})))))

(defn- session-events
  "Prompts, tool calls, stops and SessionEnd of a main transcript."
  [f]
  (let [es (lines f)
        sid (or (some #(get (second %) "sessionId") es) (str (fs/strip-ext (fs/file-name f))))
        cwd-at (fn [i] (or (some #(get (second %) "cwd") (rseq (subvec es 0 (inc i)))) (some #(get (second %) "cwd") es)))
        turn-end (fn [{:keys [last-ts stop-ts text]} i]
                   (event :stop (or stop-ts last-ts) f (first (nth es i))
                          {"session_id" sid "hook_event_name" "Stop" "last_assistant_message" (or text "")}))
        step (fn [{:keys [open seen] :as st} [i [n e]]]
               (let [p (prompt-of e)
                     task (when p (tag p "task-id"))]
                 (cond
                   (and p (not (and task (seen task))))
                   (-> st
                       (update :out into (concat (when open [(turn-end open (max 0 (dec i)))])
                                                 [(event :prompt (ms (get e "timestamp")) f n
                                                         {"session_id" sid "cwd" (cwd-at i)
                                                          "hook_event_name" "UserPromptSubmit" "prompt" p}
                                                         {:branch (get e "gitBranch")})]))
                       (assoc :open {:last-ts (ms (get e "timestamp"))})
                       (cond-> task (update :seen conj task)))

                   (and open (= "assistant" (get e "type")))
                   (let [t (texts e)]
                     (cond-> (assoc-in st [:open :last-ts] (ms (get e "timestamp")))
                       (not (str/blank? t)) (assoc-in [:open :text] t)))

                   (and open (= "system" (get e "type")) (= "stop_hook_summary" (get e "subtype")))
                   (assoc-in st [:open :stop-ts] (ms (get e "timestamp")))

                   :else st)))
        {:keys [out open]} (reduce step {:out [] :seen #{}} (map-indexed vector es))
        last-ts (some #(ms (get (second %) "timestamp")) (rseq es))]
    (concat out
            (when open [(turn-end open (dec (count es)))])
            (when (seq es)
              [(event :stop last-ts f (first (peek es))
                      {"session_id" sid "hook_event_name" "SessionEnd" "reason" "replay"})])
            (tool-events f es sid nil))))

(defn- agent-events [session-file]
  (let [sid (str (fs/strip-ext (fs/file-name session-file)))
        dir (fs/path (fs/parent session-file) sid "subagents")]
    (when (fs/directory? dir)
      (mapcat
       (fn [f]
         (let [id (second (re-find #"agent-(.+)\.jsonl$" (str f)))
               meta (let [m (fs/path dir (str "agent-" id ".meta.json"))]
                      (when (fs/exists? m) (json/parse-string (slurp (str m)))))
               es (lines f)
               a {"agent_id" id "agent_type" (or (get meta "agentType") "")}
               ts (keep #(ms (get (second %) "timestamp")) es)
               last-text (some #(let [t (when (= "assistant" (get (second %) "type")) (texts (second %)))]
                                  (when-not (str/blank? t) t))
                               (rseq es))]
           (when (seq es)
             (concat
              [(event :subagent-start (first ts) f (first (first es))
                      (merge {"session_id" sid "cwd" (some #(get (second %) "cwd") es)
                              "hook_event_name" "SubagentStart"} a)
                      {:branch (some #(get (second %) "gitBranch") es)})]
              (tool-events f es sid a)
              [(event :subagent-stop (last ts) f (first (peek es))
                      (merge {"session_id" sid "hook_event_name" "SubagentStop"
                              "last_assistant_message" (or last-text "")} a))]))))
       (sort (map str (fs/glob dir "agent-*.jsonl")))))))

(def ^:private order {:prompt 0 :subagent-start 1 :tool 2 :subagent-stop 3 :stop 4})

(defn events
  "Every event of the given main transcripts and their agents, in time order."
  [files]
  (->> files
       (mapcat #(concat (session-events %) (agent-events %)))
       (filter :ts)
       (sort-by (juxt :ts #(order (:kind %)) :file :line))))

;; --- worktrees named by the transcripts ---------------------------------------------

(defn worktree-adds
  "Orig dirs that `git worktree add` calls created: [dir], resolved against the call's cwd
  (and a leading `cd x &&`, or `git -C x`)."
  [evs]
  (for [{:keys [kind payload]} evs :when (= kind :tool)
        :let [cmd (str (get-in payload ["tool_input" "command"]))]
        [_ pre c-dir args] (re-seq #"(?:cd\s+(\S+)\s*&&\s*)?git\s+(?:-C\s+(\S+)\s+)?worktree\s+add\s+([^;&|\n]+)" cmd)
        :let [toks (remove #(re-find #"^\d?>" %) (str/split (str/trim args) #"\s+"))
              path (loop [[t & more] toks]
                     (cond (nil? t) nil
                           (#{"-b" "-B" "--reason"} t) (recur (rest more))
                           (str/starts-with? t "-") (recur more)
                           :else t))
              base (reduce (fn [b d] (if d (str (fs/path b d)) b))
                           (get payload "cwd") [pre c-dir])]
        :when (and path base)]
    (str (fs/normalize (fs/path base (str/replace path #"^['\"]|['\"]$" ""))))))

;; --- what the transcript proves about the files --------------------------------------

(defn- split-keep [s] (let [ls (str/split (str s) #"\n" -1)] (if (= "" (peek ls)) [(pop ls) true] [ls false])))

(defn patch
  "Apply structured-patch hunks to old (nil = no file). [new-content-or-nil mismatches]."
  [old hunks]
  (let [[ls nl?] (if old (split-keep old) [[] true])
        ls (vec ls)
        {:keys [out i bad nl?]}
        (reduce (fn [{:keys [i] :as st} {:strs [oldStart lines]}]
                  (let [to (max i (dec (or oldStart 1)))
                        st (-> st (update :out into (subvec ls (min i (count ls)) (min to (count ls)))) (assoc :i to))]
                    (reduce (fn [{:keys [i] :as st} l]
                              (let [c (first l) body (subs l (min 1 (count l)))]
                                (case c
                                  \+ (update st :out conj body)
                                  \\ (assoc st :nl? false)
                                  (-> st (update :i inc)
                                      (cond-> (= c \space) (update :out conj body))
                                      (cond-> (not= body (get ls i)) (update :bad inc))))))
                            st lines)))
                {:out [] :i 0 :bad 0 :nl? nl?} hunks)
        out (into out (subvec ls (min i (count ls))))]
    [(when (or (seq out) (some #(some (fn [l] (str/starts-with? l "+")) (get % "lines")) hunks))
       (str (str/join "\n" out) (when (and nl? (seq out)) "\n")))
     bad]))

(defn- edited
  "The file after an edit call, from the transcript, or nil."
  [sh {:strs [tool_name tool_input]} r]
  (let [base #(or (when (map? r) (get r "originalFile")) (shadow/content sh %))
        repl (fn [s {:strs [old_string new_string replace_all]}]
               (cond (or (nil? s) (nil? old_string) (nil? new_string)) s
                     replace_all (str/replace s old_string new_string)
                     :else (str/replace-first s old_string new_string)))
        p (get tool_input "file_path")]
    (when (and p (map? r))
      (case (str/replace (str tool_name) #"^mcp__.*__" "")
        "Write" [p (or (get r "content") (get tool_input "content"))]
        "Edit" (when-let [b (base p)] [p (repl b tool_input)])
        "MultiEdit" (when-let [b (base p)] [p (reduce repl b (get tool_input "edits"))])
        nil))))

(defn- stat! [st k] (swap! st update k (fnil inc 0)))

(defn- evidence!
  "Write into the shadow what a tool event proves. Counted in st."
  [sh st {:keys [payload result ts]}]
  (when-not (= "PostToolUseFailure" (get payload "hook_event_name"))
    (when-let [[p c] (edited sh payload result)]
      (when (and c (shadow/write! sh p c)) (stat! st :edit)))
    (doseq [{:strs [filePath hunks]} (when (map? result) (get-in result ["bashEditDiff" "files"]))]
      (shadow/sync! sh filePath ts nil)
      (when (shadow/inside? sh filePath)
        (let [[c bad] (patch (shadow/content sh filePath) hunks)]
          (if c (shadow/write! sh filePath c) (shadow/delete! sh filePath))
          (stat! st :bash-diff)
          (when (pos? bad) (stat! st :bash-diff-mismatch)))))
    (when-let [{:strs [filePath content numLines totalLines startLine]}
               (when (and (map? result) (= "text" (get result "type"))) (get result "file"))]
      (when (and filePath (string? content) (= 1 startLine) (= numLines totalLines)
                 (shadow/inside? sh filePath) (not= content (shadow/content sh filePath)))
        (shadow/write! sh filePath content)
        (stat! st :read-fix)))))

;; --- run ------------------------------------------------------------------------------

(defn- git-date [ms]
  (let [i (Instant/ofEpochMilli ms)
        off (.getTotalSeconds (.getOffset (.getRules (ZoneId/systemDefault)) i))]
    (format "%d %s%02d%02d" (quot ms 1000) (if (neg? off) "-" "+") (quot (abs off) 3600) (quot (mod (abs off) 3600) 60))))

(defn- file-of [{:strs [tool_input]}] (some #(get tool_input %) ["file_path" "path" "notebook_path"]))

(defn replay!
  "Replay main transcripts files into repo; maps {orig-root repo-dir}. Returns stats."
  [files repo maps]
  (let [evs (events files)
        sh (shadow/create (str repo "-wt") maps)
        st (atom {})]
    (doseq [wt (distinct (worktree-adds evs))] (shadow/worktree! sh wt))
    (binding [g/*repo* repo
              rec/*local* #(shadow/local sh %)
              rec/*orig* #(shadow/orig sh %)]
      (doseq [{:keys [kind ts payload branch] :as ev} evs]
        (let [cwd (get payload "cwd")]
          (when cwd
            (when-not (shadow/sync! sh cwd ts branch :dir? true) (stat! st :no-repo)))
          (when (= kind :tool)
            (when-let [f (file-of payload)]
              (when (fs/absolute? f) (shadow/sync! sh f ts branch)))
            (evidence! sh st ev))
          (binding [g/*env* {"GIT_AUTHOR_DATE" (git-date ts) "GIT_COMMITTER_DATE" (git-date ts)}]
            (rec/handle! (name kind) payload))
          (stat! st kind))))
    (merge @st {:head (shadow/stats sh)})))

(defn -main [& argv]
  (let [{:keys [sel repo maps force]}
        (loop [[a b & more :as as] argv o {:maps {}}]
          (cond (empty? as) o
                (= a "--repo") (recur more (assoc o :repo b))
                (= a "--map") (recur more (update o :maps conj (str/split b #"=" 2)))
                (= a "--force") (recur (rest as) (assoc o :force true))
                :else (recur (rest as) (assoc o :sel a))))
        repo (str (fs/absolutize (or repo default-repo)))
        files (when sel (jr/files sel))]
    (cond
      (empty? files) (do (println "usage: bb trace:replay <sel> [--repo dir] [--map orig=dir]... [--force]"
                                  (if sel (str "\nno transcripts for " sel) "")) (System/exit 1))
      (some #{repo} (map #(str (fs/absolutize %)) [g/default-repo g/*repo*]))
      (do (println "refusing to replay into the live trace repo" repo) (System/exit 1))
      (and (fs/exists? repo) (not force))
      (do (println repo "exists; --force replaces it (and" (str repo "-wt") ")") (System/exit 1))
      :else
      (do (when (fs/exists? repo) (fs/delete-tree repo) (fs/delete-tree (str repo "-wt")))
          (println "replaying" (count files) "sessions into" repo)
          (doseq [f files] (println " " f))
          (let [s (replay! files repo maps)]
            (println (pr-str s)))))))
