(ns trace.runs-test
  "trace.runs: two arms of one task (projects t--a and t--b, a payload-fed session each, as Copilot's hook
  makes them) become one comparison: tasks, the numbers per arm, the cost, the end state check and the pages.
  Throwaway repos under <reus>/.scratch/runs-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [trace.git :as g]
            [trace.health :as health]
            [trace.read :as r]
            [trace.record :as rec]
            [trace.runs :as runs]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "runs-test")))

(use-fixtures :each (fn [t] (binding [health/*claude-dir* (str (fs/path scratch "no-claude"))] (t))))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))

(defn- project! [name]
  (let [d (str (fs/path scratch name))]
    (fs/create-dirs d)
    (sh d "git" "init" "-q" "-b" "main")
    (spit (str (fs/path d ".reus-check")) "test -f ok.txt && echo fine\n")
    (sh d "git" "add" ".")
    (sh d "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "start")
    d))

(defn- arm! [name sid makes-ok? usage]
  (let [d (project! name)]
    (rec/handle! "prompt" {"session_id" sid "cwd" d "prompt" "make ok.txt" "hook_event_name" "UserPromptSubmit"})
    (when makes-ok? (spit (str (fs/path d "ok.txt")) "ok\n"))
    (rec/handle! "tool" {"session_id" sid "cwd" d "hook_event_name" "PostToolUse" "tool_name" "Write"
                         "tool_input" {"file_path" (str (fs/path d "ok.txt")) "content" "ok\n"} "tool_response" "created"
                         "model" "claude-haiku-5-5" "duration_ms" 250})
    (rec/handle! "stop" {"session_id" sid "cwd" d "hook_event_name" "Stop" "last_assistant_message" "done"
                         "model" "claude-haiku-5-5" "usage" usage})))

(deftest what-a-call-was
  (is (= :run (runs/step-kind {:kind :read :trailers {"Tool" "Bash"}})) "a shell call that names one file is not a read")
  (is (= :read (runs/step-kind {:kind :run :trailers {"Tool" "Read"}})))
  (is (= :edit (runs/step-kind {:kind :run :trailers {"Tool" "Bash"} :files [{:path "a"}]})) "a call that changed files is an edit")
  (is (= :edit (runs/step-kind {:kind :read :trailers {"Tool" "Write"}}))))

(deftest two-arms-one-task
  (fs/delete-tree scratch)
  (fs/create-dirs scratch)
  (binding [g/*repo* (str (fs/path scratch "traces"))]
    (arm! "t--cheap" "aaaaaaaa-1" true {:input_tokens 1000 :output_tokens 2000 :cache_read_input_tokens 100000 :cache_creation_input_tokens 0 :reasoning_tokens 50})
    (arm! "t--dud" "bbbbbbbb-1" false {:input_tokens 10 :output_tokens 10})
    (let [tips (r/tips)]
      (is (= {"t" {"cheap" "t--cheap" "dud" "t--dud"}} (into {} (runs/tasks tips))))
      (testing "the numbers of an arm"
        (let [a (runs/arm-data tips "cheap" "t--cheap")]
          (is (= 1 (count (:prompts a))))
          (is (= 1 (get-in a [:total :calls])))
          (is (= 1 (get-in a [:total :edits])))
          (is (= 2000 (get-in a [:total :usage :output_tokens])))
          (is (= 50 (get-in a [:total :usage :reasoning_tokens])))
          (testing "cost: tokens at the model's list price"
            (is (< (Math/abs (- (get-in a [:total :cost]) (/ (+ (* 0.10 1000) (* 0.50 2000) (* 0.01 100000)) 1e6))) 1e-12)))
          (testing "the end state passes the check in the arm's own tree, once"
            (let [c (runs/check (:tip a))]
              (is (zero? (:exit c)))
              (is (str/includes? (:out c) "fine"))
              (is (fs/exists? (fs/path g/*repo* ".git" "runs-check" (str (:tip a) ".edn")))))))
        (let [c (runs/check (:tip (runs/arm-data tips "dud" "t--dud")))]
          (is (pos? (:exit c)) "the arm that made no ok.txt fails its check")))
      (testing "the pages"
        (is (= 200 (:status (runs/route "/runs"))))
        (let [body (:body (runs/route "/runs/t"))]
          (is (str/includes? body "t--cheap")) ; the open-in-trace link names the project
          (is (str/includes? body "✓"))
          (is (str/includes? body "✗")))
        (is (= 404 (:status (runs/route "/runs/nope")))))
      (testing "a second run of an arm: both are listed, the newest is shown, ?pick= shows the older"
        (let [d (str (fs/path scratch "t--cheap"))]
          (rec/handle! "prompt" {"session_id" "cccccccc-2" "cwd" d "prompt" "again" "hook_event_name" "UserPromptSubmit"})
          (rec/handle! "stop" {"session_id" "cccccccc-2" "cwd" d "hook_event_name" "Stop" "last_assistant_message" "second"}))
        (let [tips (r/tips)
              {:keys [all shown]} (runs/task-runs tips "t" #{})
              cheap (fn [rs] (filter #(= "cheap" (:arm %)) rs))]
          (is (= 3 (count all)))
          (is (= 2 (count shown)) "one run per arm")
          (is (= "cccccccc" (:sid8 (first (cheap shown)))) "the newest")
          (is (= "aaaaaaaa" (:sid8 (first (cheap (:shown (runs/task-runs tips "t" #{"cheap:aaaaaaaa"})))))) "or the one picked"))
        (is (str/includes? (:body (runs/route "/runs/t" {})) "show"))
        (testing "the static report: one file, one inline script (expand all), no address, deltas, the whole change"
          (let [html (runs/static-html "t")]
            (is (str/includes? html "✓"))
            (is (= 1 (count (re-seq #"<script" html))))
            (is (not (re-find #"<script[^>]*src" html)))
            (is (not (re-find #"https?://" html)))
            (is (str/includes? html "Expand all"))
            (is (str/includes? html "Compared with"))
            (is (str/includes? html "The whole change"))
            (is (str/includes? html "ok.txt"))))))))
