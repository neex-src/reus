(ns trace.runner-test
  "trace.runner and trace.copilot-install without a Copilot: a stand-in copilot (test/guide/fake-copilot.bb) logs
  its words. Throwaway dirs under <reus>/.scratch/runner-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.copilot-install :as ci]
            [trace.git :as g]
            [trace.ide :as ide]
            [trace.runner :as run]))

(def code (str (fs/parent (System/getProperty "babashka.config"))))
(def scratch (str (fs/path code ".scratch" "runner-test")))

(deftest arms
  (is (= {:arm "claude-haiku-4.5-low" :model "claude-haiku-4.5" :effort "low"} (run/parse-arm "claude-haiku-4.5:low")))
  (is (= {:arm "fast" :model "gpt-5" :effort nil} (run/parse-arm "fast=gpt-5")))
  (is (= {:arm "default-high" :model nil :effort "high"} (run/parse-arm "default:high")))
  (is (= {:arm "ide-haiku-low" :model "haiku" :effort "low" :engine :ide} (run/parse-arm "ide:haiku:low")))
  (is (= {:arm "x" :model "auto" :effort nil :engine :ide} (run/parse-arm "x=ide:auto"))))

(deftest a-task-from-a-folder-and-a-prompts-file
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs (fs/path proj "src"))
    (fs/create-dirs (fs/path proj "node_modules" "x"))
    (spit (str (fs/path proj "src" "a.js")) "1")
    (spit (str (fs/path proj "node_modules" "x" "i.js")) "no")
    (spit (str (fs/path scratch "prompts.md")) "first\nprompt\n---\nsecond\n")
    (with-redefs [g/home (str (fs/path scratch "reus"))]
      (let [t (run/make-task! {:task "t1" :start (str proj) :prompts (str (fs/path scratch "prompts.md")) :check "echo hi"})]
        (is (= 2 (:prompts t)))
        (is (= "first\nprompt\n" (slurp (str (fs/path (:dir t) "prompts" "01.md")))))
        (is (fs/exists? (fs/path (:dir t) "start" "src" "a.js")))
        (is (not (fs/exists? (fs/path (:dir t) "start" "node_modules"))) "node_modules is not part of the snapshot")
        (is (= "echo hi\n" (slurp (str (fs/path (:dir t) "start" ".reus-check")))))))))

(deftest an-arm-is-one-resumed-session-in-a-fresh-copy
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj") log (str (fs/path scratch "fake.log"))]
    (fs/create-dirs proj)
    (spit (str (fs/path proj "a.txt")) "a")
    (spit (str (fs/path scratch "p.md")) "one\n---\ntwo\n---\nthree\n")
    (with-redefs [g/home (str (fs/path scratch "reus"))]
      (run/make-task! {:task "t2" :start (str proj) :prompts (str (fs/path scratch "p.md"))})
      (binding [run/*copilot-bin* (str (fs/path code "test" "guide" "fake-copilot.bb"))
                run/*copilot-home* (str (fs/path scratch "no-copilot-home"))]
        (let [seen (atom [])
              r (p/shell {:out :string :err :string :extra-env {"FAKE_LOG" log}} "true")
              res (with-redefs [p/shell (let [orig p/shell] (fn [opts & args] (apply orig (update opts :extra-env assoc "FAKE_LOG" log) args)))]
                    (run/run-arm! "t2" (run/parse-arm "m1=claude-haiku-5-5:high") #(swap! seen conj %)))
              calls (map edn/read-string (str/split-lines (slurp log)))]
          (is (:ok? res))
          (is (= 3 (:prompts res)))
          (is (= ["-p" "one"] (take 2 (first calls))))
          (is (= 1 (count (distinct (map #(second (drop-while (partial not= "--session-id") %)) calls)))) "one session id for every prompt")
          (is (every? #(some #{"--trust-folder"} %) calls) "no hooks in the Copilot home: the arm gets its own, so the folder is trusted")
          (is (every? #(= "claude-haiku-5-5" (second (drop-while (partial not= "--model") %))) calls))
          (is (every? #(= "high" (second (drop-while (partial not= "--reasoning-effort") %))) calls))
          (is (fs/exists? (fs/path (:dir res) ".github" "hooks" "reus.json")))
          (is (fs/exists? (fs/path (:dir res) "a.txt")) "a copy of the start folder")
          (is (= 1 (count (str/split-lines (:out (p/shell {:dir (:dir res) :out :string} "git" "log" "--oneline"))))) "one start commit"))))))

(deftest install-into-a-copilot-home
  (fs/delete-tree scratch)
  (let [h (str (fs/path scratch "copilot-home"))]
    (ci/install! h)
    (let [hooks (json/parse-string (slurp (str (fs/path h "hooks" "reus.json"))))
          skill (slurp (str (fs/path h "skills" "reus-compare" "SKILL.md")))]
      (is (= 1 (get hooks "version")))
      (is (= #{"UserPromptSubmit" "PostToolUse" "PostToolUseFailure" "Stop" "SessionEnd" "SubagentStart" "SubagentStop"} (set (keys (get hooks "hooks")))))
      (is (str/includes? (get-in hooks ["hooks" "Stop" 0 "bash"]) "-m trace.copilot"))
      (is (str/includes? skill (str g/home)))
      (is (not (str/includes? skill "{{REUS}}")))
      (is (str/starts-with? skill "---\nname: reus-compare")))
    (ci/uninstall! h)
    (is (not (fs/exists? (fs/path h "hooks" "reus.json"))))
    (is (not (fs/exists? (fs/path h "skills" "reus-compare"))))))

(deftest byok-key-from-a-local-file
  (fs/delete-tree (fs/path scratch "key"))
  (fs/create-dirs (fs/path scratch "key"))
  (let [a (str (fs/path scratch "key" "a.env")) b (str (fs/path scratch "key" "b.env"))]
    (spit a "# a comment\nOTHER=1\n")
    (spit b "OTHER=1\nexport ANTHROPIC_API_KEY=\"sk-test-123\"\n")
    (is (= "sk-test-123" (#'ide/key-in-file b)))
    (is (nil? (#'ide/key-in-file a)) "no key line")
    (is (nil? (#'ide/key-in-file (str (fs/path scratch "key" "none.env")))) "no file")
    (when-not (or (System/getenv "REUS_IDE_BYOK_KEY") (System/getenv "ANTHROPIC_API_KEY"))
      (binding [ide/*key-files* [a b]] (is (= "sk-test-123" (ide/byok-key)) "the first file that has one")))))
