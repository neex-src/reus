(ns trace.cost-test
  "trace.cost over throwaway transcripts, a trace written by trace.record and a git repo under <reus>/.scratch/cost-test:
  a message belongs to the project of its own cwd and is counted once, the cache write is split, the CLI's cost-state is
  compared, traces and transcripts are joined by session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [trace.cost :as cost]
            [trace.git :as g]
            [trace.record :as rec]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "cost-test")))
(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))
(defn- jsonl! [f es] (fs/create-dirs (fs/parent f)) (spit (str f) (apply str (map #(str (json/generate-string %) "\n") es))))

(defn- asst [id cwd model out ts & [extra]]
  {"type" "assistant" "cwd" cwd "timestamp" ts
   "message" {"id" id "model" model "content" [{"type" "text" "text" "x"}]
              "usage" (merge {"input_tokens" 2 "output_tokens" out "cache_read_input_tokens" 1000 "cache_creation_input_tokens" 100
                              "cache_creation" {"ephemeral_1h_input_tokens" 100 "ephemeral_5m_input_tokens" 0}} extra)}})
(defn- prompt [cwd ts text] {"type" "user" "cwd" cwd "timestamp" ts "message" {"content" text}})

(deftest counts
  (fs/delete-tree scratch)
  (let [A (str (fs/path scratch "work" "alpha")) B (str (fs/path scratch "work" "beta")) elsewhere (str (fs/path scratch "work"))
        projects {"alpha" {:roots [A]} "beta" {:roots [B]}}
        dir (fs/path scratch "claude" "projects" "-w")
        main (fs/path dir "aaaaaaaa-0000-0000-0000-000000000001.jsonl")
        sub (fs/path dir "aaaaaaaa-0000-0000-0000-000000000001" "subagents" "agent-a1b2.jsonl")
        again (fs/path dir "bbbbbbbb-0000-0000-0000-000000000002.jsonl")]
    (jsonl! main [(prompt A "2026-11-04T10:00:00Z" "do alpha")
                  (asst "m1" A "claude-sonnet-5-5" 10 "2026-11-04T10:00:05Z")
                  (assoc (asst "m1" A "claude-sonnet-5-5" 10 "2026-11-04T10:00:06Z") "note" "a second line of the same message")
                  (asst "m2" elsewhere "claude-opus-5-5" 20 "2026-11-04T10:01:00Z")
                  (asst "m3" B "claude-sonnet-5-5" 30 "2026-11-05T01:00:00Z")
                  {"type" "cost-state" "totalCostUSD" 1.5}])
    (jsonl! sub [(asst "s1" B "claude-haiku-5-5" 40 "2026-11-05T02:00:00Z")])
    (jsonl! again [(asst "m1" A "claude-sonnet-5-5" 10 "2026-11-04T10:00:05Z") (asst "m9" A "claude-sonnet-5-5" 5 "2026-11-06T10:00:00Z")]) ; m1 copied by a resume
    (let [t (cost/scan-transcripts [(str (fs/path scratch "claude" "projects"))] projects (java.time.ZoneId/of "UTC"))
          a (get-in t [:projects "alpha"]) b (get-in t [:projects "beta"])]
      (testing "a message is counted once, in the project of its own cwd"
        (is (= 15 (get-in a [:by-model "claude-sonnet-5-5" :usage :output_tokens])) "m1 10 + m9 5; m1 again and its second line not")
        (is (= 30 (get-in b [:by-model "claude-sonnet-5-5" :usage :output_tokens])))
        (is (= 40 (get-in b [:by-model "claude-haiku-5-5" :usage :output_tokens])) "the subagent run's message")
        (is (= #{[ "aaaaaaaa-0000-0000-0000-000000000001" "a1b2"]} (:agents b)))
        (is (= 1 (:prompts a)))
        (is (= #{"2026-11-04" "2026-11-06"} (:days a)))
        (is (= 20 (get-in t [:elsewhere :by-model "claude-opus-5-5" :usage :output_tokens])) "a message in no project is kept apart")
        (is (= 1.5 (get-in t [:cli "aaaaaaaa-0000-0000-0000-000000000001"])))))
    (testing "the cost is the list price with the cache write split: sonnet 5.5 m1"
      (let [res (cost/count-all {:zone "UTC" :transcripts [(str (fs/path scratch "claude" "projects"))] :traces [] :projects projects})
            sonnet (get-in res [:projects "alpha" :by-model "claude-sonnet-5-5"])]
        (is (< (Math/abs (- (get-in res [:projects "alpha" :cost-usd])
                            (/ (+ (* 2 2 2) (* 15 10) (* 2 1000 0.1) (* 2 100 4) (* 2 2 0) (* 5 0) 0) 1e6)))
               1e-6))
        (is (= 2 (:messages sonnet)))
        (is (= 2 (get-in res [:projects "alpha" :sessions] 2)))))))

(deftest generate-calls-as-the-cli-reported
  (fs/delete-tree scratch)
  (fs/create-dirs (fs/path scratch "run"))
  (spit (str (fs/path scratch "run" "usage-1.edn"))
        (pr-str {:model "claude-haiku-5-5" :session-id "s" :took-ms 15277 :usage {:input 49536 :output 2364 :input-fresh 8 :cache-read 35294 :cache-write 14234 :cost-usd 0.00438254}}))
  (let [[c] (cost/generate-calls [(str (fs/path scratch "run"))])]
    (is (= [49536 2364 0.00438254 15.277] ((juxt :tokens-in :tokens-out :cost-usd :seconds) c)))
    (is (re-find #"usage-1.edn$" (:source c)))))

(deftest git-and-traces
  (fs/delete-tree scratch)
  (let [repo (fs/path scratch "repo") z (java.time.ZoneId/of "UTC")]
    (fs/create-dirs repo)
    (sh repo "git" "init" "-q" "-b" "main")
    (spit (str (fs/path repo "a.txt")) "1\n2\n3\n")
    (sh repo "git" "add" ".")
    (sh repo "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "one")
    (spit (str (fs/path repo "a.txt")) "1\n2\n")
    (sh repo "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-am" "two")
    (let [s (cost/git-stats repo z)]
      (is (= 2 (:commits s)))
      (is (= 3 (:added s)))
      (is (= 1 (:deleted s))))
    (testing "a trace branch is a session of the project its root is in; its tokens from the response commit"
      (let [proj (str (fs/path scratch "work" "alpha")) tr (str (fs/path scratch "s.jsonl"))
            sid "cccccccc-0000-0000-0000-000000000003"]
        (fs/create-dirs proj)
        (sh proj "git" "init" "-q" "-b" "main")
        (jsonl! tr [{"type" "user" "timestamp" "2026-11-05T07:00:00Z" "promptId" "pp" "message" {"content" "go"}}
                    (asst "z1" proj "claude-haiku-5-5" 7 "2026-11-05T07:00:05Z")])
        (binding [g/*repo* (str (fs/path scratch "traces"))]
          (rec/handle! "prompt" {"session_id" sid "cwd" proj "prompt" "go" "transcript_path" tr})
          (rec/handle! "stop" {"session_id" sid "cwd" proj "transcript_path" tr "prompt_id" "pp" "last_assistant_message" "x"}))
        (let [st (cost/scan-traces (str (fs/path scratch "traces")) {"alpha" {:roots [proj]}} z)]
          (is (= 1 (get-in st [:projects "alpha" :prompts])))
          (is (= [["cccccccc" "alpha"]] (keys (:sess st))))
          (is (= 7 (get-in st [:sess ["cccccccc" "alpha"] :tally :usage :output_tokens]))))))))
