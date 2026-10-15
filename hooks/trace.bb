#!/usr/bin/env bb
(ns trace
  "Claude Code hook recorder: a session's work becomes git history, nothing squashed.

  Usage (hook command, hook JSON on stdin, <worktree> = a git worktree whose
  checked-out branch is the session's main branch, e.g. `sane`):
    trace.bb <worktree> prompt   UserPromptSubmit
    trace.bb <worktree> tool     PostToolUse / PostToolUseFailure
    trace.bb <worktree> stop     Stop

  One prompt = one branch <main>-pNN, cut from <main>'s tip:
    1. `prompt NN` -- doto.md holds the prompt text.
    2. one commit per tool call, in execution order:
       * the call changed files: the commit IS that change (the diff is the
         effect); subject = the bash command (or tool + target), body = the full
         command, and after a `--- error ---` delimiter the error output if it failed.
       * the call changed nothing (a read, a REPL eval, a test run, a curl): the
         result is written to reads/<path of the one file it read>, replacing the
         file; commit subject = the blob sha of that file as it was at that moment
         (git cat-file recovers it), body = the command. A call that names no
         single file stores its result in reads/_cmd/NN.k and has subject `NN.k`.
         A failure is written into the same file after the delimiter.
    3. `response NN` -- doto.md replaced by the assistant's reply.
  The branch is then merged into <main> with --no-ff. <main> ends with exactly one
  merge commit per prompt; the commits between are what the session did.

  doto.md and reads/ are written through the index only (plumbing) and are
  sparse-excluded, so they never appear in the session's working tree, and the
  session cannot grep them.

  Parallel hook invocations are serialised by a mkdir lock in the worktree's git
  dir. All state is in git: the current branch name says which prompt we are in."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def ^:private ident ["-c" "user.name=reus-trace" "-c" "user.email=trace@reus.local"])
(def ^:private delim "--- error ---")

(defn- git [wt & args]
  (let [opts (if (map? (last args)) (last args) {})
        args (if (map? (last args)) (butlast args) args)
        r (apply p/shell (merge {:out :string :err :string :continue true :dir wt} opts)
                 "git" (concat ident args))]
    (assoc r :out (str/trim-newline (:out r)))))

(defn- with-lock [wt f]
  (let [lock (fs/path (str/trim (:out (git wt "rev-parse" "--absolute-git-dir"))) "trace.lock")]
    (loop [n 0]
      (if (try (fs/create-dir lock) true (catch Exception _ false))
        (try (f) (finally (fs/delete-tree lock)))
        (if (< n 600) (do (Thread/sleep 100) (recur (inc n))) (f))))))

(defn- branch [wt] (:out (git wt "branch" "--show-current")))
(defn- prompt-branch? [b] (boolean (re-find #"-p\d+$" b)))
(defn- main-of [b] (str/replace b #"-p\d+$" ""))
(defn- prompt-no [b] (second (re-find #"-p(\d+)$" b)))
(defn- dirty? [wt] (not (str/blank? (:out (git wt "status" "--porcelain")))))
(defn- clip [s n] (let [s (str/replace (str s) #"\s+" " ")] (if (> (count s) n) (str (subs s 0 n) "...") s)))
(defn- first-line [s n] (clip (first (str/split-lines (str s))) n))

(defn- put!
  "Record content at path in the index only (never on disk) and keep it out of the tree."
  [wt path content]
  (let [sha (:out (git wt "hash-object" "-w" "--stdin" {:in (str content)}))]
    (git wt "update-index" "--add" "--cacheinfo" (str "100644," sha "," path))
    (git wt "update-index" "--skip-worktree" path)
    (fs/delete-if-exists (fs/path wt path))
    sha))

(defn- commit! [wt subject body]
  (git wt "commit" "-q" "--allow-empty" "-m" subject "-m" (str body)))

;; --- tool results ---------------------------------------------------------

(defn- text-of
  "Best-effort text of a tool response, whatever its shape."
  [r]
  (cond
    (nil? r) ""
    (string? r) r
    (sequential? r) (str/join "\n" (map text-of r))
    (map? r) (let [parts (keep #(let [v (get r %)] (when-not (nil? v) (text-of v)))
                               ["stdout" "stderr" "text" "content" "output" "result" "file" "error"])]
               (if (seq parts) (str/join "\n" (remove str/blank? parts)) (json/generate-string r)))
    :else (str r)))

(defn- failed? [{:strs [hook_event_name tool_response]} text]
  (or (= hook_event_name "PostToolUseFailure")
      (and (map? tool_response) (true? (get tool_response "is_error")))
      (boolean (when-let [[_ n] (re-find #"(?i)exit code:?\s*(\d+)" (subs text 0 (min 80 (count text))))]
                 (not= n "0")))))

(defn- command-of [{:strs [tool_name tool_input]}]
  (or (get tool_input "command") (get tool_input "code")
      (str tool_name " " (json/generate-string tool_input))))

(defn- target-of
  "The one existing file inside the worktree this call read, else nil."
  [wt {:strs [tool_name tool_input cwd]}]
  (let [root (str (fs/canonicalize wt))
        inside (fn [tok] (let [tok (str/replace tok #"^['\"]|['\"]$" "")
                               f (cond (str/blank? tok) nil
                                       (fs/absolute? tok) tok
                                       :else (some #(let [c (str (fs/path % tok))] (when (fs/exists? c) c)) [root (or cwd root)]))]
                           (when (and f (fs/regular-file? f))
                             (let [c (str (fs/canonicalize f))]
                               (when (str/starts-with? c (str root "/")) (subs c (inc (count root))))))))]
    (if-let [fp (get tool_input "file_path")]
      (inside fp)
      (when-let [cmd (get tool_input "command")]
        (let [files (distinct (keep inside (str/split cmd #"[\s;|&<>()]+")))]
          (when (= 1 (count files)) (first files)))))))

;; --- events ---------------------------------------------------------------

(defn- reply-of [{:strs [last_assistant_message transcript_path]}]
  (or (when-not (str/blank? last_assistant_message) last_assistant_message)
      (when (and transcript_path (fs/exists? transcript_path))
        (->> (str/split-lines (slurp transcript_path))
             (keep #(let [m (try (json/parse-string %) (catch Exception _ nil))]
                      (when (= "assistant" (get m "type"))
                        (let [c (get-in m ["message" "content"])]
                          (when (sequential? c)
                            (str/join "\n" (keep (fn [b] (when (= "text" (get b "type")) (get b "text"))) c)))))))
             (remove str/blank?)
             last))
      ""))

(defn- k-of [wt b] (parse-long (or (not-empty (:out (git wt "rev-list" "--count" (str (main-of b) ".." b)))) "0")))

(defn- close-prompt! [wt reply]
  (let [b (branch wt)]
    (when (prompt-branch? b)
      (when (dirty? wt)
        (git wt "add" "-A")
        (commit! wt "leftover changes at end of turn" ""))
      (let [first-msg (first (str/split-lines (:out (git wt "log" "--reverse" "--format=%s" (str (main-of b) ".." b)))))]
        (put! wt "doto.md" reply)
        (commit! wt (str "response " (prompt-no b)) "")
        (git wt "checkout" "-q" (main-of b))
        (git wt "merge" "-q" "--no-ff" "-m" (or first-msg b) b)))))

(defn- next-n [wt main]
  (let [names (str/split-lines (:out (git wt "branch" "--list" (str main "-p*") "--format=%(refname:short)")))]
    (inc (reduce max -1 (keep #(some-> (re-find #"-p(\d+)$" %) second parse-long) names)))))

(defn- on-prompt [wt {:strs [prompt]}]
  (close-prompt! wt "")
  (let [main (branch wt) n (next-n wt main)]
    (git wt "checkout" "-q" "-b" (format "%s-p%02d" main n))
    (put! wt "doto.md" prompt)
    (commit! wt (format "prompt %02d: %s" n (clip prompt 100)) "")))

(defn- on-tool [wt {:strs [tool_name tool_input tool_response] :as in}]
  (let [b (branch wt)]
    (when (prompt-branch? b)
      (let [cmd (command-of in)
            text (text-of tool_response)
            text (if (= (get in "hook_event_name") "PostToolUseFailure") (or (get in "error") text) text)
            bad? (failed? in text)
            errpart (when bad? (str "\n\n" delim "\n" text))]
        (if (dirty? wt)
          (do (git wt "add" "-A")
              (commit! wt (if (get tool_input "command") (first-line cmd 100)
                              (str tool_name ": " (first-line (or (get tool_input "file_path") cmd) 90)))
                       (str cmd errpart)))
          (let [n (prompt-no b)
                rel (target-of wt in)
                content (if bad? (str delim "\n" text) text)]
            (if rel
              (let [blob (:out (git wt "hash-object" (str (fs/path wt rel))))]
                (put! wt (str "reads/" rel) content)
                (commit! wt blob cmd))
              (let [k (k-of wt b)]
                (put! wt (format "reads/_cmd/%s.%d" n k) content)
                (commit! wt (format "%s.%d" n k) cmd)))))))))

(let [[wt event] *command-line-args*
      input (try (json/parse-string (slurp *in*)) (catch Exception _ {}))]
  (with-lock wt
    #(case event
       "prompt" (on-prompt wt input)
       "tool" (on-tool wt input)
       "stop" (close-prompt! wt (reply-of input))
       (binding [*out* *err*] (println "trace.bb: unknown event" event)))))
