(ns trace.export-test
  "trace.export over a trace written by trace.record: a session of one prompt that reads a file, changes one with a command and
  ends, plus pure checks of the kinds, the redaction and the private paths. Throwaway repos under <reus>/.scratch/export-test."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.export :as ex]
            [trace.git :as g]
            [trace.record :as rec]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "export-test")))
(def sid "1b2c3d4e-0000-4000-8000-000000000001")

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))

(defn- record-session! [proj]
  (let [base {"session_id" sid "cwd" proj}
        f (str (fs/path proj "notes.txt"))]
    (rec/handle! "prompt" (assoc base "prompt" "Make notes.txt say hello, mail me at someone@example.org"))
    (rec/handle! "tool" (assoc base "tool_name" "Read" "tool_use_id" "u1" "tool_input" {"file_path" f} "tool_response" {"file" {"content" "old"}} "duration_ms" 5))
    (spit f "hello\n")
    (rec/handle! "tool" (assoc base "tool_name" "Bash" "tool_use_id" "u2" "tool_input" {"command" "echo hello > notes.txt"} "tool_response" {"stdout" "done"} "duration_ms" 7))
    (rec/handle! "stop" (assoc base "last_assistant_message" "Done, notes.txt says hello."))))

(defn- all-text [dir] (apply str (map #(slurp (str %)) (filter fs/regular-file? (fs/glob dir "**")))))

(deftest kinds
  (is (= :prompt (ex/kind-of {:base :prompt :text "do it"})))
  (is (= :notice-agent (ex/kind-of {:base :prompt :text "<agent-message from=\"a1\"> x"})))
  (is (= :brief (ex/kind-of {:base :prompt :text "do it" :agent? true})))
  (is (= :notice-task (ex/kind-of {:base :prompt :text "<task-notification>\n<task-id>"})))
  (is (= :end (ex/kind-of {:base :response})))
  (is (= :agent (ex/kind-of {:base :tool :tool "Agent"})))
  (is (= :handback (ex/kind-of {:base :tool :tool "SubagentHandback"})))
  (is (= :write (ex/kind-of {:base :tool :tool "Bash" :real? true})))
  (is (= :read (ex/kind-of {:base :tool :tool "Read"})))
  (is (= :command (ex/kind-of {:base :tool :tool "Bash"}))))

(deftest private-paths-and-redaction
  (is (ex/private? ".claude-trace/claude-md/~__.claude__CLAUDE.md"))
  (is (ex/private? ".claude-trace/memory/x.md"))
  (is (not (ex/private? "src/claude-trace/x.clj")))
  (let [[f counts] (ex/redactor [{:re "[\\w.]+@[\\w.]+\\.\\w+" :to "[address cut]" :why "an e-mail address"}])]
    (is (= "mail [address cut] and [address cut]" (f "mail a@b.org and c@d.com")))
    (is (= {"an e-mail address" #{"a@b.org" "c@d.com"}} @counts))))

(deftest message-without-trailers
  (is (= "cmd" (ex/strip-trailers "cmd\n\nTool: Bash\nCwd: /a")))
  (is (= "just text" (ex/strip-trailers "just text"))))

(deftest trailers-in-order
  (is (= [["Tool" "Bash"] ["File" "x"] ["Cwd" "/a"]] (ex/trailer-list "cmd\n\nTool: Bash\nFile: x\nCwd: /a")))
  (is (= [] (ex/trailer-list "just text"))))

(deftest a-session-as-files
  (fs/delete-tree scratch)
  (let [proj (str (fs/path scratch "proj" "demo")) out (str (fs/path scratch "out"))]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "notes.txt")) "old\n")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (record-session! proj)
      (let [res (ex/export! "1b2c3d4e" out {:redact [{:re "[\\w.]+@[\\w.]+\\.\\w+" :to "[address cut]" :why "an e-mail address"}]})
            g* (json/parse-string (slurp (str (fs/path out "graph.json"))) true)
            idx (slurp (str (fs/path out "index.html")))
            p0 (slurp (str (fs/path out "p" "00.html")))
            text (all-text out)
            kinds (mapv :kind (:commits g*))]
        (is (= 4 (:commits res)) "prompt, Read, Bash, end")
        (is (= ["prompt" "read" "write" "end"] kinds))
        (testing "a commit that changed files has its diff, the read has none"
          (let [w (nth (:commits g*) 2) r (nth (:commits g*) 1)]
            (is (= [{:path "notes.txt" :add 1 :del 1}] (:files w)))
            (is (str/includes? (get-in w [:diff :text]) "+hello"))
            (is (nil? (:diff r)))
            (is (pos? (:tree-files w)))
            (is (= "Bash" (:tool w)))
            (is (contains? (:notes w) :event))
            (is (str/includes? (get-in w [:notes :event :text]) "echo hello"))))
        (testing "the pages are self-contained"
          (is (not (re-find #"<script|https?://|@import|<link" text)))
          (is (str/includes? p0 "echo hello &gt; notes.txt"))
          (is (str/includes? idx "p/00.html")))
        (testing "the cut is made, counted and said"
          (is (not (str/includes? text "someone@example.org")))
          (is (= {"an e-mail address" 1} (:cuts res)))
          (is (str/includes? idx "1 × an e-mail address")))
        (testing "nothing of .claude-trace leaves"
          (is (not (str/includes? text ".claude-trace/")))
          (let [tip (g/git "rev-parse" "refs/heads/demo/s/1b2c3d4e")
                snap (remove str/blank? (str/split-lines (g/git "ls-tree" "-r" "--name-only" tip)))
                priv (filter #(re-find #"^\.claude-trace/(claude-md|memory)/" %) snap)
                lines (for [f priv, l (str/split-lines (g/git "show" (str tip ":" f))) :when (>= (count (str/trim l)) 25)] (str/trim l))]
            (is (every? #(not (str/includes? text %)) lines) (str (count lines) " lines of the instruction/memory snapshot checked"))))))))
