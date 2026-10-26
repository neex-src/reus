(ns trace.agents
  "What a subagent run was, from its transcript. The trace repo only has the Agent
  call and its launch note (which names the agent id); the run itself is in
  ~/.claude/projects/<dir>/<session>/subagents/agent-<id>.jsonl, with
  agent-<id>.meta.json beside it (agentType, description, model alias).

  (agent-id output)       the id in an Agent call's output, else nil
  (info session id input) {:model :effort :type :description :ms :calls :running?}
                          or nil when the transcript is gone. :effort is the call's
                          effort input, else the `Effort: x` the brief states."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Duration Instant]))

(def ^:private projects-dir (str (fs/path (fs/home) ".claude" "projects")))

(defn agent-id [output]
  (second (or (re-find #"\"agentId\"\s*:\s*\"([A-Za-z0-9]+)\"" (str output))
              (re-find #"agentId: ([A-Za-z0-9]+)" (str output)))))

(defn effort-of
  "The effort an Agent call asked for: its input, else the brief's own `Effort: high` line."
  [input]
  (or (get input "effort")
      (some-> (re-find #"(?i)\beffort:\s*(extra[- ]high|xhigh|max|high|medium|low)\b" (str (get input "prompt")))
              second str/lower-case (str/replace "-" " "))))

(defn- ms-between [a b]
  (try (.toMillis (Duration/between (Instant/parse a) (Instant/parse b))) (catch Exception _ nil)))

(defn find-file
  "The file of agent id of a session: beside the session's transcript when its path is known (no
  search), else found among all projects' transcripts."
  [session id suffix & [transcript]]
  (or (when transcript
        (let [f (fs/path (fs/parent transcript) (str session) "subagents" (str "agent-" id suffix))]
          (when (fs/exists? f) f)))
      (first (fs/glob projects-dir (str "*/" session "/subagents/agent-" id suffix)))))

(defn- scan
  "{:first :last :calls :model} of a transcript, from its raw lines."
  [f]
  (with-open [r (io/reader (str f))]
    (reduce (fn [acc l]
              (let [ts (second (re-find #"\"timestamp\":\"([^\"]+)\"" l))
                    m (when-not (:model acc) (second (re-find #"\"message\":\{\"model\":\"(claude-[^\"]+)\"" l)))]
                (cond-> acc
                  ts (-> (update :first #(or % ts)) (assoc :last ts))
                  (re-find #"\"type\":\"tool_use\"" l) (update :calls inc)
                  m (assoc :model m))))
            {:calls 0} (line-seq r))))

(def ^:private cache (atom {}))

(defn- scan* [f]
  (let [k [(str f) (fs/size f) (str (fs/last-modified-time f))]]
    (or (get @cache k) (let [s (scan f)] (swap! cache assoc k s) s))))

(defn info [session id input]
  (when-let [f (and session id (find-file session id ".jsonl"))]
    (let [meta (some-> (find-file session id ".meta.json") str slurp (json/parse-string true))
          {:keys [first last calls model]} (scan* f)
          age (some-> (ms-between last (str (Instant/now))) (max 0))]
      {:model (or model (:model meta) (get input "model"))
       :effort (effort-of input)
       :type (or (:agentType meta) (get input "subagent_type"))
       :description (or (:description meta) (get input "description"))
       :ms (ms-between first last)
       :calls calls
       :running? (boolean (and age (< age 60000)))})))

;; --- task notifications -------------------------------------------------------

(defn- tag [s t] (some-> (re-find (re-pattern (str "(?s)<" t ">(.*?)</" t ">")) s) second str/trim))

(defn- unescape [s] (some-> s (str/replace "&lt;" "<") (str/replace "&gt;" ">") (str/replace "&quot;" "\"") (str/replace "&amp;" "&")))

(defn notifications
  "The task notifications a session received (a background command or monitor ended,
  an agent finished): [{:at epoch-secs :task :tool-use :status :summary :result}], in
  order. They arrive without a prompt, so no hook sees them; the transcript queues each."
  [session]
  (when-let [f (and session (first (fs/glob projects-dir (str "*/" session ".jsonl"))))]
    (with-open [r (io/reader (str f))]
      (->> (line-seq r)
           (filter #(and (str/includes? % "\"operation\":\"enqueue\"") (str/includes? % "task-notification>")))
           (keep #(try (json/parse-string % true) (catch Exception _ nil)))
           (keep (fn [{:keys [content timestamp]}]
                   (when (str/includes? (str content) "<task-id>")
                     {:at (.getEpochSecond (Instant/parse timestamp))
                      :task (tag content "task-id")
                      :tool-use (tag content "tool-use-id")
                      :status (tag content "status")
                      :summary (unescape (or (tag content "summary") (tag content "event")))
                      :result (unescape (tag content "result"))})))
           vec))))
