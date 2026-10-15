(ns jsonl.read-test
  "jsonl.read on a small transcript written under <reus>/.scratch."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [jsonl.read :as r]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "jsonl-test")))

(def lines
  [{:type "user" :sessionId "s1" :cwd "/w/proj" :timestamp "2026-01-01T00:00:00Z" :message {:content "<command-name>/model</command-name>"}}
   {:type "user" :timestamp "2026-01-01T00:00:01Z" :message {:content "fix the bug"}}
   {:type "assistant" :timestamp "2026-01-01T00:00:02Z"
    :message {:id "m1" :model "claude-x" :usage {:output_tokens 10 :input_tokens 3} :content [{:type "thinking" :thinking "hm"}]}}
   {:type "assistant" :timestamp "2026-01-01T00:00:02Z"
    :message {:id "m1" :model "claude-x" :usage {:output_tokens 10 :input_tokens 3}
              :content [{:type "tool_use" :id "t1" :name "Bash" :input {:command "ls; false"}}]}}
   {:type "user" :timestamp "2026-01-01T00:00:05Z" :toolUseResult {}
    :message {:content [{:type "tool_result" :tool_use_id "t1" :is_error true :content [{:type "text" :text "a\nb"}]}]}}
   {:type "user" :isMeta true :timestamp "2026-01-01T00:00:06Z" :message {:content "caveat"}}
   {:type "user" :timestamp "2026-01-01T00:00:07Z" :message {:content [{:type "text" :text "and again"}]}}
   {:type "assistant" :timestamp "2026-01-01T00:00:08Z"
    :message {:id "m2" :model "claude-x" :usage {:output_tokens 5 :input_tokens 1}
              :content [{:type "tool_use" :id "t2" :name "mcp__clj__clojure_eval" :input {:code "(+ 1 2)"}}]}}])

(deftest one-transcript
  (fs/create-dirs scratch)
  (let [f (str (fs/path scratch "s1.jsonl"))]
    (spit f (str (apply str (map #(str (json/generate-string %) "\n") lines)) "not json\n"))
    (let [s (r/session f)
          [c1 c2] (:calls s)]
      (is (= ["fix the bug" "and again"] (map :text (:prompts s))) "slash commands and meta entries are not prompts")
      (is (= [0 1] (map :n (:prompts s))))
      (is (= {:output_tokens 15 :input_tokens 4} (select-keys (:usage s) [:output_tokens :input_tokens]))
          "a message split over two lines counts once")
      (is (= {:n 0 :tool "Bash" :error? true :result "a\nb" :result-chars 3 :ms 3000}
             (select-keys c1 [:n :tool :error? :result :result-chars :ms])))
      (is (= 1 (:n c2)))
      (is (nil? (:ms c2)) "a call without a result has no duration")
      (is (= "mcp:clojure_eval" (r/short-tool (:tool c2))))
      (is (= "-w-proj" (:dir s)) "outside ~/.claude/projects the dir comes from the cwd")
      (is (= [f] (r/files f)))
      (is (= [f] (r/files scratch))))))
