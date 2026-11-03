(ns trace.gui-test
  "trace.gui's pages over a trace written by trace.record (the session-branch layout): every part a page
  loads lazily is there, outputs are the recorded ones, running prompts and agent runs show."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.fns :as fns]
            [trace.git :as g]
            [trace.gui :as gui]
            [trace.record :as rec])
  (:import [java.net URLDecoder]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "gui-test")))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string} args))

(defn- fresh! []
  (fs/delete-tree scratch)
  (let [proj (fs/path scratch "proj")]
    (fs/create-dirs proj)
    (sh proj "git" "init" "-q" "-b" "main")
    (spit (str (fs/path proj "a.clj")) "(ns a)\n\n(defn f [] 1)\n")
    (spit (str (fs/path proj ".gitignore")) "ignored/\n")
    (sh proj "git" "add" ".")
    (sh proj "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "init")
    (str proj)))

(defn- get* [uri]
  (let [[path q] (str/split uri #"\?" 2)]
    (@#'gui/handler {:uri (URLDecoder/decode path "UTF-8") :query-string q})))

(defn- srcs [html] (map #(str/replace % "&amp;" "&") (map second (re-seq #"data-src=\"([^\"]+)\"" html))))

(deftest pages-of-the-session-layout
  (let [proj (fresh!) a (str (fs/path proj "a.clj")) ign (str (fs/path proj "ignored" "x"))]
    (binding [g/*repo* (str (fs/path scratch "traces"))]
      (fs/create-dirs (fs/path proj "ignored"))
      (spit ign "not in the trace")
      (rec/handle! "prompt" {"session_id" "s1" "cwd" proj "prompt" "change f"})
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "tool_name" "Read" "tool_input" {"file_path" a}
                           "tool_response" {"file" {"content" "(ns a) ..."}}})
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "tool_name" "Read" "tool_input" {"file_path" ign}
                           "tool_response" {"file" {"content" "not in the trace"}}})
      (spit a "(ns a)\n\n(defn f [] 2)\n")
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "tool_name" "Edit"
                           "tool_input" {"file_path" a "old_string" "(defn f [] 1)" "new_string" "(defn f [] 2)"}
                           "tool_response" "ok"})
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "ls"}
                           "tool_response" {"stdout" "a.clj\nignored"}})
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "tool_name" "Bash" "tool_input" {"command" "true"}
                           "tool_response" {"stdout" ""}})
      (testing "a prompt still open is running, in the poll and on its page"
        (let [t (json/parse-string (:body (get* "/x/tips?m=proj")))]
          (is (= ["s1@00"] (get t "live")))
          (is (= "s1@00" (get t "latest")))
          (is (every? #(str/starts-with? % "proj/") (keys (get t "tips")))))
        (is (str/includes? (:body (get* "/p/s1%4000?m=proj")) "Still working")))
      ;; an agent whose Agent call this trace never saw (started before the hook was on)
      (rec/handle! "tool" {"session_id" "s1" "cwd" proj "agent_id" "ab12" "agent_type" "Explore"
                           "tool_name" "Bash" "tool_input" {"command" "echo agent"} "tool_response" {"stdout" "agent"}})
      (rec/handle! "subagent-stop" {"session_id" "s1" "agent_id" "ab12" "last_assistant_message" "agent done"})
      (rec/handle! "stop" {"session_id" "s1" "last_assistant_message" "f is 2 now"})
      (rec/handle! "prompt" {"session_id" "s1" "cwd" proj "prompt" "<task-notification>\n<task-id>b1</task-id>\n<status>completed</status>\n<summary>Background command \"x\" completed</summary>\n</task-notification>"})
      (rec/handle! "stop" {"session_id" "s1" "last_assistant_message" "ok"})

      (testing "the start page goes to the newest prompt of the newest session's project"
        (is (= "/p/s1%4001?m=proj" (get-in (get* "/") [:headers "location"]))))
      (let [{:keys [status body]} (get* "/p/s1%4000?m=proj")]
        (is (= 200 status))
        (testing "every lazy part of the page loads, and says something"
          (doseq [src (srcs body)]
            (let [r (get* src)]
              (is (= 200 (:status r)) src)
              (is (not (str/blank? (:body r))) src))))
        (testing "an output is the call's output"
          (is (some #(= "a.clj\nignored" (:body (get* %))) (srcs body))))
        (testing "a call with no output says so; a read file outside the recorded tree says so"
          (is (str/includes? body "No output recorded."))
          (is (str/includes? body "not in the tree recorded for this step")))
        (testing "the agent run shows under its prompt"
          (is (str/includes? body "Agent runs with no Agent step here · 1"))
          (is (str/includes? body "echo agent")))
        (testing "no longer running"
          (is (not (str/includes? body "Still working")))
          (is (empty? (get (json/parse-string (:body (get* "/x/tips?m=proj"))) "live")))))
      (testing "the net change of a prompt is its own: the edit, not the later prompts"
        (let [net (:body (get* "/x/net?m=proj&p=s1%4000"))]
          (is (str/includes? net "+(defn f [] 2)"))
          (is (not (str/includes? net ".claude-trace")))))
      (testing "a notification prompt is listed by its summary"
        (is (str/includes? (:body (get* "/p/s1%4001?m=proj")) "Notification: Background command")))
      (testing "files and the evolution of one file"
        (is (str/includes? (:body (get* "/files?m=proj")) "a.clj"))
        (let [body (:body (get* "/file?m=proj&path=a.clj"))]
          (doseq [src (srcs body)] (is (= 200 (:status (get* src))) src))))
      (testing "the fn layer: every definition, and the versions of one with the step that made each"
        (is (not (str/includes? (:body (get* "/files?m=proj")) "Functions · proj")))
        (fns/run! g/*repo*)
        (let [body (:body (get* "/fns?m=proj"))]
          (is (str/includes? body "Functions · proj"))
          (is (str/includes? body "/fn?m=proj&amp;path=a.clj%2Ff")))
        (let [body (:body (get* "/fn?m=proj&path=a.clj%2Ff"))]
          (is (str/includes? body "changed"))
          (is (str/includes? body "/p/s1%4000?m=proj#proj-s1%4000-"))
          (doseq [src (srcs body)] (is (= 200 (:status (get* src))) src))
          (is (some #(str/includes? (:body (get* %)) "+(defn f [] 2)") (filter #(str/starts-with? % "/x/bdiff") (srcs body))))))
      (testing "a missing object is a 404 that says what is missing"
        (is (= 404 (:status (get* (str "/x/show?r=" (apply str (repeat 40 "0")))))))))))
