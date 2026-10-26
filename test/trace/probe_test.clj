(ns trace.probe-test
  "trace.probe: is an event already in the transcript (a research tool; the live hook no longer
  probes), against transcripts under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [trace.probe :as probe]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "probe-test")))

(defn- line [m] (str (json/generate-string m) "\n"))
(defn- result [id] (line {"type" "user" "message" {"role" "user" "content" [{"type" "tool_result" "tool_use_id" id "content" "ok"}]}}))
(defn- say [t] (line {"type" "assistant" "message" {"role" "assistant" "content" [{"type" "text" "text" t}]}}))

(defn- fresh! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs proj)
    (p/shell {:dir (str proj) :out :string} "git" "init" "-q" "-b" "main")
    (str proj)))

(deftest tail-only
  (let [tr (str (fs/path (fresh!) "big.jsonl"))]
    (spit tr (str (result "toolu_old") (apply str (repeat 3000 (say (apply str (repeat 200 "x"))))) (say "the reply")))
    (is (true? (:found? (probe/measure "stop" {"last_assistant_message" "the reply" "transcript_path" tr}))))
    (is (true? (:found? (probe/measure "tool" {"tool_use_id" "toolu_old" "transcript_path" tr})))
        "outside the first window, found by the widened last look")
    (is (nil? (:found? (probe/measure "stop" {"transcript_path" tr}))) "nothing to look for")))
