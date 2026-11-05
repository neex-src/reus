(ns trace.turn-test
  "trace.turn on synthetic entries, and what it adds live (trace.record, against a transcript
  written by the test) and in a replay, under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.agents :as agents]
            [trace.git :as g :refer [git]]
            [trace.read :as r]
            [trace.record :as rec]
            [trace.replay :as rp]
            [trace.replay-test :as rt]
            [trace.turn :as turn]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "turn-test")))

(defn- t [s] (str "2026-01-01T11:00:" s "Z"))
(defn- usage [out] {"input_tokens" 1 "output_tokens" out "cache_creation_input_tokens" 10 "cache_read_input_tokens" 100})
(defn- a [ts mid block & [m]]
  {"type" "assistant" "timestamp" (t ts)
   "message" (merge {"id" mid "model" "m-1" "stop_reason" "tool_use" "usage" (usage 5) "content" [block]} m)})
(defn- use* [id] {"type" "tool_use" "id" id "name" "Bash" "input" {}})
(defn- res [ts id] {"type" "user" "timestamp" (t ts) "message" {"content" [{"type" "tool_result" "tool_use_id" id "content" "x"}]}})
(defn- prompt* [ts text] {"type" "user" "timestamp" (t ts) "promptId" (str "p-" ts) "message" {"content" text}})

(def entries
  [(prompt* "00.000" "do it")
   {"type" "attachment" "attachment" {"type" "date" "date" "2026-01-01"}}
   (a "01.000" "m1" {"type" "thinking" "thinking" "hmm"})
   (a "01.100" "m1" {"type" "text" "text" "let me look"})
   (a "01.200" "m1" (use* "u1"))
   (a "01.300" "m1" (use* "u2"))
   (res "02.200" "u1")
   (res "02.700" "u2")
   (a "03.000" "m2" {"type" "text" "text" "intermediate"} {"stop_reason" nil})
   (a "03.500" "m3" (use* "u3"))
   (res "04.000" "u3")
   {"type" "attachment" "attachment" {"type" "total_tokens_reminder" "text" "<total_tokens>1</total_tokens>"}}
   {"type" "attachment" "attachment" {"type" "total_tokens_reminder" "text" "<total_tokens>2</total_tokens>"}}
   {"type" "attachment" "attachment" {"type" "instructions" "files" [{"path" "/CLAUDE.md" "content" "rules"}]}}
   (a "05.000" "m4" {"type" "text" "text" "done"} {"stop_reason" "end_turn" "usage" (usage 7)})
   {"type" "system" "subtype" "turn_duration" "durationMs" 5000 "messageCount" 9}
   (prompt* "06.000" "and then?")])

(deftest pure
  (let [ix (turn/index entries)]
    (testing "a tool call: its message, once-only usage, earlier blocks, duration"
      (let [u1 (turn/tool ix "u1" {}) u2 (turn/tool ix "u2" {})]
        (is (= "m-1" (:model u1)))
        (is (= {:input_tokens 1 :output_tokens 5 :cache_creation_input_tokens 10 :cache_read_input_tokens 100} (:usage u1)))
        (is (nil? (:usage u2)))
        (is (= "u1" (:usage-of u2)))
        (is (= [{:type "thinking" :text "hmm"} {:type "text" :text "let me look"}] (:blocks u1)))
        (is (= 1000 (:duration-ms u1)))
        (is (= 1400 (:duration-ms u2)))
        (is (= 7000 (:duration-ms (turn/tool ix "u1" {:duration-ms 7000}))) "the payload's wins")))
    (testing "text written since the previous tool result comes from other messages"
      (is (= ["intermediate"] (:since-text (turn/tool ix "u3" {}))))
      (is (nil? (:since-text (turn/tool ix "u1" {})))))
    (is (nil? (turn/tool ix "nope" {})))
    (testing "a prompt: its id, what was attached since the previous prompt, the previous turn"
      (let [p (turn/prompt ix 16)]
        (is (= "p-06.000" (:prompt-id p)))
        (is (= [{:type "date" :summary {"date" "2026-01-01"} :n 1}
                {:type "total_tokens_reminder" :summary {} :n 2}
                {:type "instructions" :summary {"files" [{"path" "/CLAUDE.md" "content" "rules"}]} :n 1}]
               (:attachments p)))
        (is (= {:duration-ms 5000 :message-count 9} (:prev-turn p)))))
    (testing "a stop: usage once per message, turn_duration after it"
      (let [s (turn/stop ix 15)]
        (is (= 4 (:assistant-messages s)))
        (is (= 22 (get-in s [:usage :output_tokens])) "m1 5 (its lines repeat the usage), m2 5, m3 5, m4 7"))
      (is (= 5000 (:duration-ms (turn/stop ix 15)))))
    (testing "the turn of a promptId, in a file as it is now: up to the next prompt"
      (is (= (turn/stop ix 15) (turn/stop-of ix "p-00.000")))
      (is (= "done" (turn/last-text-of ix "p-00.000")))
      (is (nil? (turn/last-text-of ix "p-06.000")) "the last prompt has no text yet")
      (is (nil? (turn/stop-of ix "p-nope"))))
    (testing "the 1-hour part of a cache write is kept when the line says it"
      (let [e (assoc-in (a "01.000" "mx" {"type" "text" "text" "x"}) ["message" "usage" "cache_creation"] {"ephemeral_1h_input_tokens" 7 "ephemeral_5m_input_tokens" 3})]
        (is (= {:input_tokens 1 :output_tokens 5 :cache_creation_input_tokens 10 :cache_read_input_tokens 100 :cache_1h_input_tokens 7}
               (:usage (turn/stop (turn/index [(prompt* "00.000" "p") e]) 2))))))
    (is (= 16 (turn/find-prompt ix "and then?")))
    (is (= "done" (turn/last-text entries)))))

;; --- live ---------------------------------------------------------------------------

(defn- jsonl! [f es] (spit (str f) (apply str (map #(str (json/generate-string %) "\n") es))))
(defn- git-init [dir]
  (fs/create-dirs dir)
  (p/shell {:dir (str dir) :out :string :err :string} "git" "init" "-q" "-b" "main")
  (str dir))
(defn- note [ref c] (let [r (g/git* {} "notes" (str "--ref=" ref) "show" c)] (when (zero? (:exit r)) (str/trim (:out r)))))
(defn- ev [event m] (rec/handle! event m (json/generate-string m)))

(deftest live
  (fs/delete-tree scratch)
  (let [proj (git-init (fs/path scratch "proj")) tr (str (fs/path scratch "s1.jsonl"))
        out-file (str (fs/path scratch "task.output"))
        big (str (fs/path scratch "big.txt"))]
    (spit out-file "background output\n")
    (spit big "THE WHOLE OUTPUT\nline 2\n")
    (jsonl! tr (concat [(prompt* "00.000" "do it")] (take 6 (drop 2 entries))))
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (ev "prompt" {"session_id" "s1" "cwd" proj "prompt" "do it" "transcript_path" tr})
      (let [tool (fn [id m] (ev "tool" (merge {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_use_id" id "transcript_path" tr
                                               "tool_input" {"command" "ls"} "tool_response" {"stdout" "x"}} m)))]
        (tool "u1" {"duration_ms" 1234})
        (let [n (edn/read-string (note "turn" (git "rev-parse" "proj/s/s1")))]
          (is (= "m-1" (:model n)))
          (is (= 1234 (:duration-ms n)) "live, the payload's duration")
          (is (= 5 (get-in n [:usage :output_tokens]))))
        (testing "the assistant line is not written yet: found at the next hook"
          (tool "u9" {})
          (is (= {:use-found? false} (edn/read-string (note "turn" (git "rev-parse" "proj/s/s1")))))
          (spit tr (str (json/generate-string (a "09.000" "m9" (use* "u9"))) "\n") :append true)
          (tool "u1" {})
          (is (= "m9" (:message-id (edn/read-string (note "turn" (git "rev-parse" "proj/s/s1~1")))))))
        (testing "a persisted output is recorded in full"
          (tool "u1" {"tool_response" {"stdout" (str "<persisted-output>\nOutput too large. Full output saved to: " big "\n\nPreview:\nTHE WH")}})
          (is (= "THE WHOLE OUTPUT\nline 2" (str/trim (note "output" (git "rev-parse" "proj/s/s1")))))))
      (testing "a prompt that is a task notification keeps the task's output"
        (spit tr (str (json/generate-string (prompt* "10.000" (str "<task-notification><output-file>" out-file "</output-file></task-notification>"))) "\n") :append true)
        (ev "stop" {"session_id" "s1" "last_assistant_message" "bye" "transcript_path" tr})
        (ev "prompt" {"session_id" "s1" "cwd" proj "transcript_path" tr
                      "prompt" (str "<task-notification><output-file>" out-file "</output-file></task-notification>")})
        (is (= "background output" (note "output" (git "rev-parse" "proj/s/s1")))))
      (testing "the stop of a turn"
        (is (some? (edn/read-string (note "turn" (git "rev-parse" "proj/s/s1")))))))))

(deftest subagent-without-a-message
  (let [proj (git-init (fs/path scratch "proj2")) pdir (str (fs/path scratch "projects"))
        af (fs/path pdir "p" "s1" "subagents" "agent-ab12.jsonl")]
    (fs/create-dirs (fs/parent af))
    (jsonl! af [(prompt* "00.000" "brief") (a "01.000" "m1" {"type" "text" "text" "agent says"})])
    (with-redefs-fn {#'agents/projects-dir pdir}
      (fn []
        (binding [g/*repo* (str (fs/path scratch "traces2"))]
          (rec/handle! "prompt" {"session_id" "s1" "cwd" proj "prompt" "go"})
          (rec/handle! "subagent-start" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "x"})
          (rec/handle! "subagent-stop" {"session_id" "s1" "agent_id" "ab12"})
          (let [tips (r/tips)]
            (is (= "agent says" (:reply (r/prompt-data tips "proj2" "s1-ab12@00-aab12"))))))))))

;; --- replay ---------------------------------------------------------------------------

(deftest replay-notes-and-backups
  (let [[f proj] (#'rt/fixture!)
        repo (str (fs/path rt/scratch "replay-turn"))
        hist (str (fs/path rt/scratch "history"))]
    (fs/create-dirs (fs/path hist "s1"))
    (spit (str (fs/path hist "s1" "bk@v1")) "from the backup\n")
    ;; a snapshot saying a.clj was backed up before the first tool call
    (spit f (str (json/generate-string {"type" "file-history-snapshot"
                                        "snapshot" {"trackedFileBackups"
                                                    {"a.clj" {"backupFileName" "bk@v1" "backupTime" "2026-01-01T11:00:00.500Z"
                                                              "realParentDir" rt/orig}}}})
                 "\n" (slurp f)))
    (let [stats (binding [rp/*history-dir* hist] (rp/replay! [f] repo {rt/orig proj}))]
      (is (= 1 (:backup-applied stats)))
      (binding [g/*repo* repo]
        (let [tips (r/tips) d (r/prompt-data tips "proj" "s1@00")
              [rd ed] (:steps d)]
          (testing "the notes are read back: a step's, the prompt's and the stop's"
            (is (= 1000 (:duration-ms (:turn rd))))
            (is (= [] (:attachments (:turn d))))
            (is (map? (:turn-end d)))
            (is (= (count (:steps d)) (count (filter :turn (:steps d))))))
          (is (= "m2026-01-01T11:00:03.000Z" (:message-id (:turn ed)))))))))

;; --- the end of a turn written after the Stop hook (`claude -p`) ---------------------------------

(def ^:private race-scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "turn-race-test")))

(defn- race! [tag final-after-ms]
  (fs/delete-tree race-scratch)
  (let [proj (git-init (fs/path race-scratch "proj")) tr (str (fs/path race-scratch "s1.jsonl"))
        final (str (json/generate-string (a "05.000" "m4" {"type" "text" "text" "all done"} {"stop_reason" "end_turn" "usage" (usage 7)})) "\n")]
    (jsonl! tr [(prompt* "00.000" "do it") (a "01.000" "m1" (use* "u1")) (res "02.000" "u1")])
    (binding [g/*repo* (str (fs/path race-scratch "traces"))]
      (ev "prompt" {"session_id" tag "cwd" proj "prompt" "do it" "transcript_path" tr})
      (let [late (future (Thread/sleep final-after-ms) (spit tr final :append true))
            t0 (System/currentTimeMillis)]
        (ev "stop" {"session_id" tag "cwd" proj "transcript_path" tr "prompt_id" "p-00.000" "last_assistant_message" "all done"})
        (let [ms (- (System/currentTimeMillis) t0)]
          @late
          {:note (edn/read-string (note "turn" (git "rev-parse" (str "proj/s/" tag)))) :ms ms})))))

(deftest stop-waits-for-the-end-of-the-turn
  (testing "the final message reaches the transcript after the hook fired: its tokens are counted"
    (let [{:keys [note]} (race! "s1" 400)]
      (is (= 2 (:assistant-messages note)))
      (is (= 12 (get-in note [:usage :output_tokens])) "m1 5 + the late m4 7; read at the hook's size it was 5")
      (is (= 2 (get-in note [:usage :input_tokens])))))
  (testing "a message that never turns up: the turn as it is, after the wait"
    (with-redefs [rec/stop-wait-ms 300]
      (let [{:keys [note ms]} (race! "s2" 2000)]
        (is (= 1 (:assistant-messages note)))
        (is (= 5 (get-in note [:usage :output_tokens])))
        (is (< ms 1900) "it did not wait for the late message"))))
  (testing "a transcript that is complete at the hook: no wait"
    (fs/delete-tree race-scratch)
    (let [proj (git-init (fs/path race-scratch "proj")) tr (str (fs/path race-scratch "s1.jsonl"))]
      (jsonl! tr [(prompt* "00.000" "do it") (a "05.000" "m4" {"type" "text" "text" "all done"} {"stop_reason" "end_turn" "usage" (usage 7)})])
      (binding [g/*repo* (str (fs/path race-scratch "traces"))]
        (ev "prompt" {"session_id" "s3" "cwd" proj "prompt" "do it" "transcript_path" tr})
        (let [t0 (System/currentTimeMillis)]
          (ev "stop" {"session_id" "s3" "cwd" proj "transcript_path" tr "prompt_id" "p-00.000" "last_assistant_message" "all done"})
          (is (< (- (System/currentTimeMillis) t0) 2500))
          (is (= 7 (get-in (edn/read-string (note "turn" (git "rev-parse" "proj/s/s3"))) [:usage :output_tokens]))))))))

(deftest stop-sums-a-long-turn
  (testing "a turn longer than the 256 KB the hook looks at first: all its messages are summed, not those of the last 256 KB"
    (fs/delete-tree race-scratch)
    (let [proj (git-init (fs/path race-scratch "proj")) tr (str (fs/path race-scratch "s1.jsonl"))
          big {"type" "user" "timestamp" (t "03.000") "message" {"content" [{"type" "tool_result" "tool_use_id" "u1" "content" (apply str (repeat 400000 "x"))}]}}]
      (jsonl! tr [(prompt* "00.000" "do it") (a "01.000" "m1" (use* "u1")) big (a "04.000" "m2" (use* "u2")) (res "05.000" "u2")
                  (a "06.000" "m3" {"type" "text" "text" "all done"} {"stop_reason" "end_turn" "usage" (usage 7)})])
      (binding [g/*repo* (str (fs/path race-scratch "traces"))]
        (ev "prompt" {"session_id" "s4" "cwd" proj "prompt" "do it" "transcript_path" tr})
        (ev "stop" {"session_id" "s4" "cwd" proj "transcript_path" tr "prompt_id" "p-00.000" "last_assistant_message" "all done"})
        (let [n (edn/read-string (note "turn" (git "rev-parse" "proj/s/s4")))]
          (is (= 3 (:assistant-messages n)))
          (is (= 17 (get-in n [:usage :output_tokens])) "m1 5 + m2 5 + m3 7; the old window held m2 and m3 only"))))))
