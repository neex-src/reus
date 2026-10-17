(ns trace.turn
  "What a session transcript adds to a hook event: the model's side of it. Pure functions
  over a transcript's entries (parsed jsonl lines, string keys), the same live (the tail of
  the file, read at hook time) and in a replay (the whole file).

  (index entries)       the lookups the functions below share, made once per entries
  (tool ix id opts)     the assistant message that holds tool_use `id`: {:model :message-id
                        :stop-reason :usage | :usage-of <first tool_use id of the message>
                        :blocks [{:type :text}] (the message's thinking and text before the
                        call) :since-text [text] (other messages' text since the previous
                        tool result) :duration-ms}. A message spans several lines that repeat
                        its usage, so the usage is on the first tool call of the message only.
                        opts {:duration-ms} from the hook payload (live); else result ts - use ts.
  (prompt ix i)         entry i is a prompt: {:prompt-id :attachments [{:type :summary :n}] since
                        the previous prompt, :prev-turn {:duration-ms :message-count} when the
                        previous turn's turn_duration entry is there}
  (stop ix end)         the turn that ends before entry `end` (default: the last entry):
                        {:usage (summed once per message) :assistant-messages :prompt-id
                        :duration-ms :message-count} (the last two when a turn_duration entry
                        follows, which Claude Code writes after the Stop hooks)
  (find-prompt ix text) index of the last prompt-like entry (user, queued_command, queue-operation)
                        holding the text
  (last-text entries)   the last assistant text, or nil
  (read-entries lines)  entries of raw jsonl lines, the ones this ns uses"
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.time Instant]))

(def ^:private skip-line #"^\{\"type\":\"(api-request|mode|permission-mode|atis-latch|last-prompt|ai-title|custom-title|agent-name|bridge-session|cost-state|file-history)")

(defn read-entries
  "Parsed entries of raw jsonl lines, without the bookkeeping ones that are big and unused here."
  [lines]
  (into [] (keep #(when-not (re-find skip-line %) (try (json/parse-string %) (catch Exception _ nil)))) lines))

(defn- blocks [e] (let [c (get-in e ["message" "content"])] (if (sequential? c) c [])))
(defn- ms [ts] (try (.toEpochMilli (Instant/parse ts)) (catch Exception _ nil)))
(defn- msg-id [e] (get-in e ["message" "id"]))
(defn- user-text [e]
  (let [c (get-in e ["message" "content"])]
    (if (string? c) c (str/join "\n" (keep #(when (= "text" (get % "type")) (get % "text")) c)))))

(defn- real-prompt? [e]
  (and (= "user" (get e "type")) (not (get e "isMeta")) (not (get e "toolUseResult"))
       (not-any? #(= "tool_result" (get % "type")) (blocks e))
       (not (str/blank? (user-text e)))))

(defn index
  "Lookups over entries: ids of tool uses and results, message lines, and the sorted
  positions of prompts, tool results, attachments and turn_duration entries."
  [entries]
  (reduce
   (fn [ix [i e]]
     (case (get e "type")
       "assistant" (-> (update-in ix [:msg (msg-id e)] (fnil conj []) i)
                       (as-> ix (reduce #(assoc-in %1 [:use (get %2 "id")] i) ix
                                        (filter #(= "tool_use" (get % "type")) (blocks e)))))
       "user" (cond-> ix
                (real-prompt? e) (update :prompts conj i)
                true (as-> ix (reduce #(-> %1 (update :results conj i) (assoc-in [:result (get %2 "tool_use_id")] i)) ix
                                      (filter #(= "tool_result" (get % "type")) (blocks e)))))
       "attachment" (update ix :atts conj i)
       "system" (cond-> ix (= "turn_duration" (get e "subtype")) (update :durations conj i))
       ix))
   {:entries entries :msg {} :use {} :result {} :prompts (sorted-set) :results (sorted-set)
    :atts (sorted-set) :durations (sorted-set)}
   (map-indexed vector entries)))

(defn- before [s i] (first (rsubseq s < i)))
(defn- after [s i] (first (subseq s > i)))
(defn- between [s lo hi] (subseq s > lo < hi))

(def ^:private usage-keys ["input_tokens" "output_tokens" "cache_creation_input_tokens" "cache_read_input_tokens"])
(defn- usage-of [e]
  (when-let [u (get-in e ["message" "usage"])] (into {} (keep (fn [k] (when-let [v (get u k)] [(keyword k) v]))) usage-keys)))

(defn- message-usage
  "The usage of a message: from its last line."
  [{:keys [entries msg]} mid]
  (some-> (get msg mid) peek entries usage-of))

(defn tool
  "See ns. nil when the tool_use is not in the entries."
  [{:keys [entries msg use result] :as ix} id & [{:keys [duration-ms]}]]
  (when-let [i (get use id)]
    (let [e (entries i) mid (msg-id e)
          lines (get msg mid)
          first-use (some (fn [j] (some #(when (= "tool_use" (get % "type")) (get % "id")) (blocks (entries j)))) lines)
          own (take-while #(not= id (get % "id")) (blocks e))
          prior (concat (mapcat #(blocks (entries %)) (filter #(< % i) lines))
                        (remove #(= "tool_use" (get % "type")) own))
          prev-result (before (:results ix) i)
          since (for [j (range (inc (or prev-result -1)) i)
                      :let [x (entries j)]
                      :when (and (= "assistant" (get x "type")) (not= mid (msg-id x)))
                      b (blocks x) :when (= "text" (get b "type"))]
                  (get b "text"))
          done (some-> (get result id) entries (get "timestamp") ms)
          began (ms (get e "timestamp"))
          stop (some #(get-in (entries %) ["message" "stop_reason"]) (rseq lines))]
      (cond-> {:model (get-in e ["message" "model"]) :message-id mid :stop-reason stop
               :blocks (vec (keep #(case (get % "type")
                                     "thinking" {:type "thinking" :text (get % "thinking")}
                                     "text" {:type "text" :text (get % "text")}
                                     nil)
                                  prior))}
        (and (= id first-use) (message-usage ix mid)) (assoc :usage (message-usage ix mid))
        (not= id first-use) (assoc :usage-of first-use)
        (seq since) (assoc :since-text (vec since))
        (or duration-ms (and done began)) (assoc :duration-ms (or duration-ms (- done began)))))))

(def ^:private bare #{"total_tokens_reminder"})
(def ^:private keep-content #{"instructions" "file" "nested_memory" "plan_file_reference"})

(defn- clip [s n] (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) "...") s)))

(defn- summary [a]
  (let [t (get a "type")]
    (cond
      (keep-content t) (dissoc a "type")
      (bare t) {}
      :else
      (into {} (keep (fn [[k v]]
                       (cond (= k "type") nil
                             (or (string? v) (number? v) (boolean? v)) [k (if (string? v) (clip v 100) v)]
                             (sequential? v) [k (count v)]
                             :else nil)))
            a))))

(defn- attachments [{:keys [entries]} idxs]
  (->> idxs
       (map #(let [a (get (entries %) "attachment")] {:type (get a "type") :summary (summary a)}))
       (reduce (fn [acc x] (if-let [j (first (keep-indexed #(when (= (select-keys %2 [:type :summary]) x) %1) acc))]
                             (update-in acc [j :n] inc)
                             (conj acc (assoc x :n 1))))
               [])))

(defn prompt
  "See ns."
  [{:keys [entries prompts atts durations] :as ix} i]
  (let [e (entries i)
        prev (or (before prompts i) -1)
        d (first (between durations prev i))
        te (some-> d entries)]
    (cond-> {:attachments (attachments ix (between atts prev i))}
      (get e "promptId") (assoc :prompt-id (get e "promptId"))
      te (assoc :prev-turn {:duration-ms (get te "durationMs") :message-count (get te "messageCount")}))))

(defn stop
  "See ns."
  [{:keys [entries prompts durations] :as ix} & [end]]
  (let [end (or end (count entries))
        start (or (before prompts end) -1)
        mids (distinct (keep #(let [e (entries %)] (when (= "assistant" (get e "type")) (msg-id e))) (range (inc start) end)))
        usage (apply merge-with + {} (keep #(message-usage ix %) mids))
        nxt (or (after prompts (dec end)) Long/MAX_VALUE)
        d (first (between durations (dec end) nxt))
        te (some-> d entries)
        pe (some-> (get entries start) (get "promptId"))]
    (cond-> {:usage usage :assistant-messages (count mids)}
      pe (assoc :prompt-id pe)
      te (assoc :duration-ms (get te "durationMs") :message-count (get te "messageCount")))))

(defn find-prompt
  "Index of the last entry that is a prompt holding text (its first 200 chars), or nil."
  [{:keys [entries]} text]
  (let [need (let [t (str/trim (str text))] (subs t 0 (min 200 (count t))))
        has? #(and (string? %) (str/includes? % need))]
    (when-not (str/blank? need)
      (some (fn [i] (let [e (entries i)]
                      (when (case (get e "type")
                              "user" (some has? (if (string? (get-in e ["message" "content"])) [(get-in e ["message" "content"])] (keep #(get % "text") (blocks e))))
                              "attachment" (and (= "queued_command" (get-in e ["attachment" "type"])) (has? (get-in e ["attachment" "prompt"])))
                              "queue-operation" (has? (get e "content"))
                              false)
                        i)))
            (range (dec (count entries)) -1 -1)))))

(defn last-text
  "The text of the last assistant message that has any."
  [entries]
  (some #(when (= "assistant" (get % "type"))
           (let [t (str/join "\n" (keep (fn [b] (when (= "text" (get b "type")) (get b "text"))) (blocks %)))]
             (when-not (str/blank? t) t)))
        (rseq (vec entries))))
