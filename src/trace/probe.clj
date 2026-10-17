(ns trace.probe
  "Is the transcript already written up to an event when its hook fires?

  (measure event input)  looks in the transcript for the event's own evidence, at once,
                         then at 100 ms and 500 ms; returns an edn-able map
                           {:event :transcript :bytes :found? :waited-ms [:kind] [:error]}
                         :bytes is the file's size at the first look; :found? nil = nothing
                         to look for (:why says so). Never throws.
  (-main [repo])         bb trace:probe: what refs/notes/probe of a trace repo says.

  Evidence per event: tool -> a tool_result with the payload's tool_use_id (in the agent's own
  transcript for an agent call); prompt -> the text in a user entry, a queued_command
  attachment or a queue-operation (:kind says which); stop / subagent-stop -> the last
  assistant message in an assistant entry; subagent-start -> the agent's file exists.
  Only the tail of the file is read (it reaches 20+ MB), widened at the last look."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.agents :as agents]
            [trace.git :as g :refer [git]])
  (:import [java.io RandomAccessFile]))

(def looks "Milliseconds after the hook fired at which the transcript is looked at." [0 100 500])
(def windows "Bytes read from the end of the file: every look, then the last one again wider." [(* 256 1024) (* 8 1024 1024)])

(defn tail-lines
  "{:size n :lines [...]} the whole lines within the last `window` bytes of f."
  [f window]
  (with-open [r (RandomAccessFile. (str f) "r")]
    (let [size (.length r)
          start (max 0 (- size window))
          bs (byte-array (- size start))]
      (.seek r start)
      (.readFully r bs)
      (let [ls (str/split-lines (String. bs "UTF-8"))]
        {:size size :lines (vec (if (pos? start) (rest ls) ls))}))))

(defn- fragment
  "A prefix of the text's first line that JSON writes as is: to find its line without parsing it."
  [s]
  (->> (str/trim (str s)) (take-while #(and (not= % \newline) (<= 32 (int %) 126) (not (#{\" \\} %)))) (take 60) (apply str)))

(defn- parse [l] (try (json/parse-string l) (catch Exception _ nil)))
(defn- blocks [e] (let [c (get-in e ["message" "content"])] (if (sequential? c) c [])))
(defn- texts [e] (let [c (get-in e ["message" "content"])] (if (string? c) [c] (keep #(get % "text") c))))

(defn- first-hit
  "The first parsed entry from the end whose raw line contains frag and satisfies (f entry)."
  [lines frag f]
  (some #(when (str/includes? % frag) (when-let [e (parse %)] (f e))) (rseq lines)))

(defn- found-tool [id lines]
  (when-not (str/blank? id)
    (first-hit lines id #(when (and (= "user" (get % "type"))
                                    (some (fn [b] (and (= "tool_result" (get b "type")) (= id (get b "tool_use_id")))) (blocks %)))
                           {}))))

(defn- found-use
  "The assistant entry that holds tool_use id."
  [id lines]
  (when-not (str/blank? id)
    (first-hit lines id #(when (and (= "assistant" (get % "type")) (some (fn [b] (and (= "tool_use" (get b "type")) (= id (get b "id")))) (blocks %))) {}))))

(defn- found-prompt [prompt lines]
  (let [need (subs (str/trim prompt) 0 (min 200 (count (str/trim prompt))))
        has? #(and (string? %) (str/includes? % need))]
    (first-hit lines (fragment need)
               #(case (get % "type")
                  "user" (when (some has? (texts %)) {:kind "user"})
                  "attachment" (when (and (= "queued_command" (get-in % ["attachment" "type"])) (has? (get-in % ["attachment" "prompt"])))
                                 {:kind "queued_command"})
                  "queue-operation" (when (has? (get % "content")) {:kind "queue-operation"})
                  nil))))

(defn- found-reply [msg lines]
  (let [need (subs (str/trim msg) 0 (min 200 (count (str/trim msg))))]
    (first-hit lines (fragment need)
               #(when (and (= "assistant" (get % "type")) (some (fn [t] (str/includes? t need)) (texts %))) {}))))

(defn- agent-file [{:strs [session_id agent_id]}]
  (when (and session_id agent_id) (some-> (agents/find-file session_id agent_id ".jsonl") str)))

(defn- plan
  "{:file f :find (fn [lines] match)} for an event, or {:why ...} when there is nothing to look for."
  [event {:strs [transcript_path tool_use_id agent_id prompt last_assistant_message] :as in}]
  (let [own (if agent_id (agent-file in) transcript_path)]
    (case event
      "tool" (if (str/blank? tool_use_id) {:why "no tool_use_id"} {:file own :find (partial found-tool tool_use_id)})
      "prompt" (if (str/blank? prompt) {:why "empty prompt"} {:file transcript_path :find (partial found-prompt prompt)})
      "stop" (if (str/blank? last_assistant_message) {:why "no last_assistant_message"}
                 {:file transcript_path :find (partial found-reply last_assistant_message)})
      "subagent-stop" (if (str/blank? last_assistant_message) {:why "no last_assistant_message"}
                          {:file (agent-file in) :find (partial found-reply last_assistant_message)})
      "subagent-start" {:file (agent-file in) :find (constantly {})}
      {:why "not an event"})))

(defn- look
  "One look: {:bytes size-or-nil :match m-or-nil}; a missing file has no bytes."
  [{:keys [file find]} last?]
  (if (and file (fs/exists? file))
    (let [{:keys [size lines]} (tail-lines file (first windows))
          m (find lines)]
      {:bytes size :match (or m (when (and last? (> size (first windows)))
                                  (find (:lines (tail-lines file (second windows))))))})
    {}))

(defn measure
  "The probe of an event. A tool event is looked at once (Claude Code writes its tool_result
  only after the hooks return); prev-id, the tool_use_id of the previous recorded tool event
  of the same transcript, is looked for too: :previous-found? (nil when there is none)."
  [event in & [prev-id]]
  (try
    (let [t0 (System/currentTimeMillis)
          ms #(- (System/currentTimeMillis) t0)
          looks (if (= event "tool") [0] looks)]
      (loop [[at & more] looks first-look nil]
        (let [wait (- at (ms))
              _ (when (pos? wait) (Thread/sleep wait))
              pl (plan event in) ; an agent's file may only appear later
              {:keys [bytes match] :as lk} (look pl (empty? more))
              first-look (or first-look (assoc lk :file (:file pl)))]
          (cond
            (:why pl) {:event event :found? nil :why (:why pl)}
            (or match (empty? more))
            (cond-> {:event event :transcript (:file first-look) :bytes (:bytes first-look)
                     :found? (boolean match) :waited-ms (ms)}
              (:kind match) (assoc :kind (:kind match))
              (= event "tool") (assoc :use-found? (boolean (:match (look (assoc pl :find (partial found-use (get in "tool_use_id"))) true)))
                                      :previous-found?
                                      (when-not (str/blank? prev-id)
                                        (boolean (:match (look (plan event (assoc in "tool_use_id" prev-id)) true))))))
            :else (recur more first-look)))))
    (catch Throwable e {:event event :error (str (ex-message e))})))

;; --- bb trace:probe -----------------------------------------------------------------

(defn- notes [ref]
  (->> (str/split-lines (git "notes" (str "--ref=" ref) "list"))
       (keep #(let [[blob commit] (str/split % #" ")] (when commit [blob commit])))))

(defn probes
  "[{:commit sha ...probe}] of every commit with a probe note."
  []
  (let [ns* (notes "probe")
        bodies (g/cat-batch (map first ns*))]
    (for [[blob c] ns* :let [m (try (edn/read-string (get bodies blob)) (catch Exception _ nil))] :when (map? m)]
      (assoc m :commit c))))

(defn- origin
  "Session and time of a probed commit: from its event note, else the commit."
  [c]
  (let [ev (try (json/parse-string (git "notes" "--ref=event" "show" c)) (catch Exception _ nil))]
    (str (subs c 0 10) " session " (or (get ev "session_id") "?") " " (git "log" "-1" "--format=%cI" c))))

(defn- previous-row [ps]
  (let [known (remove (comp nil? :previous-found?) (filter #(contains? % :previous-found?) ps))
        uses (filter #(contains? % :use-found?) ps)]
    (str (when (seq known)
           (format "  previous tool call in the transcript at the next hook: %d of %d"
                   (count (filter :previous-found? known)) (count known)))
         (when (seq uses)
           (format "\n  the assistant entry holding the tool_use at the hook: %d of %d"
                   (count (filter :use-found? uses)) (count uses))))))

(defn- row [kind ps]
  (let [ps (remove :error ps)
        once? #(< (:waited-ms % 0) (second looks))
        late (filter #(and (:found? %) (not (once? %))) ps)]
    (format "%-15s %5d  at once %5d  after waiting %4d (max %d ms)  never %4d  n/a %4d"
            kind (count ps) (count (filter #(and (:found? %) (once? %)) ps))
            (count late) (reduce max 0 (map :waited-ms late))
            (count (filter #(false? (:found? %)) ps)) (count (filter (comp nil? :found?) ps)))))

(defn report [ps]
  (doseq [[kind group] (sort-by key (group-by :event ps))]
    (println (row kind group))
    (when-let [l (and (= kind "tool") (not (str/blank? (previous-row group))) (previous-row group))] (println l))
    (when-let [e (seq (filter :error group))] (println "   errors:" (count e) (pr-str (:error (first e))))))
  (let [never (filter #(false? (:found? %)) ps)]
    (when (seq never)
      (println "\nfirst never found:")
      (doseq [p (take 8 never)] (println " " (:event p) (origin (:commit p)))))))

(defn -main [& [repo]]
  (binding [g/*repo* (if repo (str (fs/canonicalize repo)) g/*repo*)]
    (report (probes))))
