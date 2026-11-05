(ns trace.call-test
  "trace.call: the page of one model call, over a trace written by trace.record from a `claude -p` shaped
  session (one prompt, two looks, an answer), with the transcript the numbers come from, and the CLI's own
  record of the same session (a Generate run dir) joined by ?run=. Throwaway repos under <reus>/.scratch/call-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.call :as call]
            [trace.git :as g]
            [trace.gui :as gui]
            [trace.read :as r]
            [trace.record :as rec])
  (:import [java.net URLDecoder]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "call-test")))
(def sid "6be9bc33-3ade-4a87-a56f-7cdf4452af82")

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))
(defn- jsonl! [f es] (spit (str f) (apply str (map #(str (json/generate-string %) "\n") es))))
(defn- get* [uri]
  (let [[path q] (str/split uri #"\?" 2)]
    (@#'gui/handler {:uri (URLDecoder/decode path "UTF-8") :query-string q})))

(def answer "### sqs send queue/inbox send-order\n# @attr id=ord-{{uuid}}\n{\"order\":1001}\n\n### dynamodb get table/messages order-acked\n# @until $.status.S == ACKED within 30s\n{\"Key\": {\"id\": {\"S\": \"ord-{{uuid}}\"}}}")
(def prompt "Write Deliberator steps for the block order-acked of prose.scn: Send an order to the inbox, wait until it is acked.\n\n# Writing steps\n\nlong instructions\n\n## The request\n\nThe description to turn into steps:\n\nSend an order to the inbox, wait until it is acked, check that service A logged it.\n\nNow Read `universal.json` here, and answer with the steps only.")

(defn- msg [ts mid block u]
  {"type" "assistant" "timestamp" (str "2026-11-05T07:24:" ts "Z")
   "message" {"id" mid "model" "claude-haiku-5-5" "stop_reason" (if (= "text" (get block "type")) "end_turn" "tool_use")
              "usage" (merge {"cache_creation" {"ephemeral_1h_input_tokens" (get u "cache_creation_input_tokens")}} u) "content" [block]}})

(defn- fresh! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj" "ctx")]
    (fs/create-dirs proj)
    (sh (fs/parent proj) "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "universal.json")) (apply str (repeat 100 "{\"op\":\"x\"}\n")))
    (sh (fs/parent proj) "git" "add" ".")
    (sh (fs/parent proj) "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (str proj)))

(defn- record-call! [proj tr]
  (let [uj (str (fs/path proj "universal.json"))
        base {"session_id" sid "cwd" proj "transcript_path" tr}]
    (jsonl! tr [{"type" "user" "timestamp" "2026-11-05T07:24:36Z" "promptId" "pp-1" "message" {"content" prompt}}
                (msg "37.500" "m1" {"type" "tool_use" "id" "u1" "name" "Read" "input" {"file_path" uj}}
                     {"input_tokens" 2 "output_tokens" 316 "cache_read_input_tokens" 2633 "cache_creation_input_tokens" 2324})
                {"type" "user" "timestamp" "2026-11-05T07:24:38Z" "message" {"content" [{"type" "tool_result" "tool_use_id" "u1" "content" "x"}]}}
                (msg "49.000" "m2" {"type" "text" "text" answer}
                     {"input_tokens" 6 "output_tokens" 2048 "cache_read_input_tokens" 32661 "cache_creation_input_tokens" 11910})])
    (rec/handle! "prompt" (assoc base "prompt" prompt))
    (rec/handle! "tool" (assoc base "tool_name" "Read" "tool_use_id" "u1" "tool_input" {"file_path" uj "offset" 0 "limit" 100}
                               "tool_response" {"file" {"content" "{\"op\":\"x\"}"}} "duration_ms" 13))
    (rec/handle! "stop" (assoc base "prompt_id" "pp-1" "last_assistant_message" answer))))

(deftest one-call
  (let [proj (fresh!) tr (str (fs/path scratch "s.jsonl"))
        run (str (fs/path scratch "run"))]
    (fs/create-dirs (fs/path run "context"))
    (spit (str (fs/path run "generate.edn")) "{:session-id \"6be9bc33-3ade-4a87-a56f-7cdf4452af82\"}")
    (spit (str (fs/path run "usage-1.edn"))
          (pr-str {:model "claude-haiku-5-5" :session-id sid :took-ms 15277 :exit 0
                   :usage {:input 49536 :output 2364 :input-fresh 8 :cache-read 35294 :cache-write 14234 :cost-usd 0.00438254 :turns 7}}))
    (spit (str (fs/path run "context" "universal.json")) (apply str (repeat 3617 "0123456789")))
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (record-call! proj tr)
      (testing "reus counts what the CLI counted"
        (let [c (call/call-data {:tips (r/tips) :branch "proj/s/6be9bc33" :nn "00"})]
          (is (= {:input_tokens 8 :output_tokens 2364 :cache_read_input_tokens 35294 :cache_creation_input_tokens 14234 :cache_1h_input_tokens 14234}
                 (:usage c)))
          (is (< (Math/abs (- 0.00438254 (:cost c))) 1e-9))))
      (testing "the page, from the trace alone"
        (let [{:keys [status body]} (get* "/call/6be9bc33")]
          (is (= 200 status))
          (is (str/includes? body "haiku-5-5"))
          (is (str/includes? body "49,536"))
          (is (str/includes? body "35,294"))
          (is (str/includes? body "2,364"))
          (is (str/includes? body "$0.0044"))
          (is (str/includes? body "Send an order to the inbox, wait until it is acked, check that service A logged it."))
          (is (str/includes? body "universal.json"))
          (is (str/includes? body "<b>2</b> steps"))
          (is (str/includes? body "dynamodb get table/messages order-acked"))
          (is (not (re-find #"<script|https?://(?!127)" body)) "self-contained")))
      (testing "?run= brings the CLI's own numbers and the size of the universal JSON"
        (let [{:keys [body]} (get* (str "/call/6be9bc33?run=" run))]
          (is (str/includes? body "36,170"))
          (is (str/includes? body "reus counted the same tokens"))
          (is (str/includes? body "15 s"))))
      (testing "a run of another session is not joined"
        (spit (str (fs/path run "usage-1.edn")) (pr-str {:session-id "other" :usage {:output 1}}))
        (is (not (str/includes? (:body (get* (str "/call/6be9bc33?run=" run))) "the CLI"))))
      (testing "the list"
        (let [{:keys [body]} (get* "/calls")]
          (is (str/includes? body "/call/6be9bc33?m=proj"))))
      (testing "no such call"
        (is (= 404 (:status (get* "/call/00000000"))))))))
