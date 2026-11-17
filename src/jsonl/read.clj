(ns jsonl.read
  "Claude Code session transcripts as data.

  Claude Code writes one jsonl file per session: ~/.claude/projects/<dir>/<session>.jsonl ($CLAUDE_CONFIG_DIR/projects),
  where <dir> is the session's cwd with / turned into -. A subagent's run goes to
  <dir>/<session>/subagents/agent-<id>.jsonl. Every line is one entry:
    user        a prompt, or tool results (content blocks of type tool_result)
    assistant   one content block (text, thinking, tool_use) of a model message; a message
                spans several lines with the same message id, each repeating its usage,
                so usage is counted once per message id here
    system, attachment, mode, ...   bookkeeping, ignored

  (files sel)    the transcripts sel names
  (session f)    one transcript as {:id :file :dir :cwd :agent? :start :end
                 :prompts [{:n :text :ts}]
                 :calls [{:n :id :tool :input :ts :ms :error? :result :result-chars}]
                 :messages [{:id :n :model :usage :ts}] :usage {summed}}
  :n is the prompt a call or message belongs to (0-based; -1 before the first prompt)."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Duration Instant]))

(def claude-dir "Claude Code's own dir: $CLAUDE_CONFIG_DIR, else ~/.claude."
  (str (or (System/getenv "CLAUDE_CONFIG_DIR") (fs/path (fs/home) ".claude"))))

(def projects-dir (str (fs/path claude-dir "projects")))

(def ^:dynamic *result-max*
  "Characters of a tool result kept in :result (:result-chars has the full size)."
  2000)

(defn agent-file? [f] (str/includes? (str f) "/subagents/"))

(defn- jsonl-under [dir] (sort (map str (fs/glob dir "**.jsonl"))))

(defn files
  "Transcripts selected by sel:
    nil            every main session under ~/.claude/projects
    a .jsonl file  that file
    a dir          every *.jsonl under it (subagent runs included)
    a session id   that session (a prefix is enough)
    anything else  the main sessions of every project dir whose name contains it
                   (\"reus\" matches -home-user-projects-reus and its sub dirs)
  agents? adds the subagent runs of the selected sessions."
  [sel & {:keys [agents?]}]
  (let [mains (fn [dirs] (sort (mapcat #(map str (fs/glob % (if agents? "**.jsonl" "*.jsonl"))) dirs)))
        pdirs (filter fs/directory? (fs/list-dir projects-dir))]
    (cond
      (nil? sel) (mains pdirs)
      (and (fs/regular-file? sel) (str/ends-with? sel ".jsonl")) [(str (fs/canonicalize sel))]
      (fs/directory? sel) (jsonl-under (fs/canonicalize sel))
      :else (or (not-empty (filter #(str/starts-with? (str (fs/file-name %)) sel) (mains pdirs)))
                (mains (filter #(str/includes? (str (fs/file-name %)) sel) pdirs))))))

(defn entries
  "The parsed lines of a transcript; lines that are not JSON are skipped."
  [f]
  (with-open [r (io/reader (str f))]
    (into [] (keep #(try (json/parse-string % true) (catch Exception _ nil))) (line-seq r))))

(defn- blocks [e] (let [c (get-in e [:message :content])] (if (sequential? c) c [])))

(defn- text-of [c]
  (cond (string? c) c
        (sequential? c) (str/join "\n" (keep #(when (= "text" (:type %)) (:text %)) c))
        :else ""))

(defn prompt-text
  "The text of an entry that is a prompt typed by the user, else nil. Slash commands,
  their output, `!` commands, agent notifications and compaction summaries are not prompts."
  [e]
  (when (and (= "user" (:type e)) (not (:isMeta e)) (not (:isCompactSummary e)) (not (:toolUseResult e))
             (not-any? #(= "tool_result" (:type %)) (blocks e)))
    (let [t (str/trim (text-of (get-in e [:message :content])))]
      (when-not (or (str/blank? t)
                    (re-find #"^<(command-name|command-message|local-command|bash-input|bash-stdout|task-notification)" t))
        t))))

(defn- ms-between [a b]
  (try (.toMillis (Duration/between (Instant/parse a) (Instant/parse b))) (catch Exception _ nil)))

(defn- add-usage [a b]
  (merge-with + a (select-keys b [:input_tokens :output_tokens :cache_read_input_tokens :cache_creation_input_tokens])))

(defn dir-of
  "The project dir name of a transcript: its dir under ~/.claude/projects, else its
  session's cwd spelled the same way (/ -> -)."
  [f cwd]
  (let [d (fs/parent (if (agent-file? f) (fs/parent (fs/parent f)) f))]
    (if (or (= (str (fs/parent d)) projects-dir) (nil? cwd))
      (str (fs/file-name d))
      (str/replace cwd "/" "-"))))

(defn session [f]
  (let [es (entries f)
        state (reduce
               (fn [{:keys [n] :as st} e]
                 (case (:type e)
                   "user"
                   (if-let [t (prompt-text e)]
                     (-> st (update :n inc) (update :prompts conj {:n (inc n) :text t :ts (:timestamp e)}))
                     (reduce (fn [st b]
                               (if-let [i (and (= "tool_result" (:type b)) (get-in st [:idx (:tool_use_id b)]))]
                                 (let [txt (text-of (:content b))]
                                   (update-in st [:calls i] merge
                                              {:error? (boolean (:is_error b))
                                               :result (subs txt 0 (min *result-max* (count txt)))
                                               :result-chars (count txt)
                                               :ms (ms-between (get-in st [:calls i :ts]) (:timestamp e))}))
                                 st))
                             st (blocks e)))
                   "assistant"
                   (let [m (:message e)
                         st (if (:id m)
                              (assoc-in st [:msgs (:id m)] {:id (:id m) :n n :model (:model m) :usage (:usage m) :ts (:timestamp e)})
                              st)]
                     (reduce (fn [st b]
                               (if (= "tool_use" (:type b))
                                 (-> st
                                     (assoc-in [:idx (:id b)] (count (:calls st)))
                                     (update :calls conj {:n n :id (:id b) :tool (:name b) :input (:input b) :ts (:timestamp e)}))
                                 st))
                             st (blocks e)))
                   st))
               {:n -1 :prompts [] :calls [] :idx {} :msgs {}} es)
        msgs (sort-by :ts (vals (:msgs state)))
        tss (keep :timestamp es)]
    {:id (or (some :sessionId es) (str (fs/strip-ext (fs/file-name f))))
     :file (str f)
     :dir (dir-of f (some :cwd es))
     :cwd (some :cwd es)
     :agent? (agent-file? f)
     :start (first tss) :end (last tss)
     :prompts (:prompts state)
     :calls (:calls state)
     :messages (vec msgs)
     :usage (reduce add-usage {} (map :usage msgs))}))

(defn short-tool
  "mcp__<server>__<tool> -> mcp:<tool>."
  [t]
  (str/replace (str t) #"^mcp__.+?__" "mcp:"))

(defn project-name
  "A project dir name without the home prefix: -home-user-projects-reus -> projects-reus."
  [dir]
  (let [home (str (str/replace (str (fs/home)) "/" "-") "-")]
    (if (str/starts-with? dir home) (subs dir (count home)) dir)))

(defn sessions
  "Every selected transcript as a session, files that fail to parse skipped."
  [sel & opts]
  (keep #(try (session %) (catch Exception e (binding [*out* *err*] (println "skip" % (ex-message e)))))
        (apply files sel opts)))

(defn args
  "Task args: [sel] plus flags --agents (subagent runs too) and --each (one table per session)."
  [argv]
  {:sel (first (remove #(str/starts-with? % "--") argv))
   :agents? (boolean (some #{"--agents"} argv))
   :each? (boolean (some #{"--each"} argv))})

(defn short-id [id] (let [id (str id)] (subs id 0 (min 8 (count id)))))
