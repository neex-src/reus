(ns trace.dash-test
  "bb gui (trace.dash, trace.found) over a made-up machine under <reus>/.scratch/dash-test: a Claude dir with two
  projects (one session replays: a project repo, an Edit, an Agent), a Copilot home with an IntelliJ session that has
  a log and a CLI one that has none, a settings.json with somebody else's hook. Looking writes nothing; the page says
  what is there; a POST without this run's token or from another host is refused; a build makes the repo in place of
  the old one, with its progress; the viewer serves it; the hooks go on and off through trace.install and
  trace.copilot-install and leave the settings as they were."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [trace.copilot-install :as ci]
            [trace.dash :as dash]
            [trace.found :as found]
            [trace.git :as g]
            [trace.health :as health]
            [trace.install :as install]
            [trace.read :as rd]
            [trace.replay :as rp]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "dash-test")))
(def claude (str (fs/path scratch "claude")))
(def copilot (str (fs/path scratch "copilot")))
(def proj (str (fs/path scratch "real" "proj")))
(def settings (str (fs/path claude "settings.json")))
(def replays {:claude-replay (str (fs/path scratch "var" "replay")) :copilot-replay (str (fs/path scratch "var" "copilot-replay"))})

(defn- sh [dir env & args] (apply p/shell {:dir (str dir) :out :string :err :string :extra-env env} args))

(defn- jsonl! [f entries]
  (fs/create-dirs (fs/parent f))
  (spit (str f) (str/join "\n" (map json/generate-string entries))))

(def sid "d0d0cafe-1111-2222-3333-444455556666")
(def csid "c0c0beef-1111-2222-3333-444455556666")

(defn- u [ts m] (merge {"type" "user" "timestamp" ts "cwd" proj "sessionId" sid "gitBranch" "main"} m))
(defn- a [ts content] {"type" "assistant" "timestamp" ts "cwd" proj "sessionId" sid "message" {"id" (str "m" ts) "content" content}})
(defn- use* [id tool input] {"type" "tool_use" "id" id "name" tool "input" input})
(defn- res [id text] {"message" {"content" [{"type" "tool_result" "tool_use_id" id "content" text}]}})

(def other-settings {"model" "opus" "hooks" {"Stop" [{"hooks" [{"type" "command" "command" "say done"}]}]}})

(defn- machine!
  "The made-up machine."
  []
  (fs/delete-tree scratch)
  (fs/create-dirs proj)
  (sh proj {} "git" "init" "-q" "-b" "main")
  (spit (str (fs/path proj "a.clj")) "v1\n")
  (sh proj {} "git" "add" ".")
  (sh proj {"GIT_AUTHOR_DATE" "2026-01-01T10:00:00Z" "GIT_COMMITTER_DATE" "2026-01-01T10:00:00Z"}
      "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" "one")
  (let [pdir (fs/path claude "projects" (str/replace proj "/" "-"))]
    (jsonl! (fs/path pdir (str sid ".jsonl"))
            [(u "2026-01-01T11:00:00.000Z" {"message" {"content" "make it v2"}})
             (a "2026-01-01T11:00:01.000Z" [(use* "t1" "Edit" {"file_path" (str proj "/a.clj") "old_string" "v1" "new_string" "v2"})])
             (u "2026-01-01T11:00:02.000Z" (merge (res "t1" "ok") {"toolUseResult" {"filePath" (str proj "/a.clj") "originalFile" "v1\n"}}))
             (a "2026-01-01T11:00:03.000Z" [(use* "t2" "Agent" {"prompt" "check"})])
             (u "2026-01-01T11:00:04.000Z" (merge (res "t2" "agentId: ag1") {"toolUseResult" {"agentId" "ag1"}}))
             (a "2026-01-01T11:00:05.000Z" [{"type" "text" "text" "it is v2"}])
             {"type" "system" "subtype" "stop_hook_summary" "timestamp" "2026-01-01T11:00:06.000Z"}])
    (jsonl! (fs/path pdir sid "subagents" "agent-ag1.jsonl")
            [{"type" "user" "timestamp" "2026-01-01T11:00:03.500Z" "cwd" proj "sessionId" sid "isSidechain" true "message" {"content" "check"}}
             {"type" "assistant" "timestamp" "2026-01-01T11:00:03.800Z" "cwd" proj "sessionId" sid
              "message" {"content" [{"type" "text" "text" "fine"}]}}]))
  (jsonl! (fs/path claude "projects" "-elsewhere-x" "e1.jsonl")
          [{"type" "user" "timestamp" "2026-01-02T11:00:00.000Z" "cwd" "/elsewhere/x" "sessionId" "e1" "message" {"content" "hi"}}])
  (spit settings (json/generate-string other-settings {:pretty true}))
  (let [dir (fs/path copilot "session-state" csid)
        n (atom 0)
        e (fn [ts type data & [extra]] (merge {"type" type "data" data "id" (str "e" (swap! n inc)) "timestamp" (str "2026-01-03T" ts "Z")} extra))
        hook (fn [ts kind input] (e ts "hook.start" {"hookType" kind "input" input}))]
    (jsonl! (fs/path dir "events.jsonl")
            [(e "10:00:00.000" "session.start" {"sessionId" csid "selectedModel" "gpt-5" "context" {"cwd" proj}})
             (hook "10:00:01.000" "userPromptSubmitted" {"prompt" "say v3"})
             (e "10:00:01.100" "user.message" {"content" "say v3"} {"id" "u1"})
             (e "10:00:02.000" "tool.execution_start" {"toolCallId" "t1" "toolName" "edit" "arguments" {"path" (str proj "/a.clj") "old_str" "v1" "new_str" "v3"}})
             (e "10:00:02.100" "tool.execution_complete" {"toolCallId" "t1" "success" true "result" {"content" "ok"}})
             (e "10:00:03.000" "assistant.message" {"content" "v3 it is" "toolRequests" [] "model" "gpt-5"})
             (hook "10:00:03.100" "agentStop" {"stopReason" "end_turn"})
             (hook "10:00:03.200" "sessionEnd" {"reason" "complete"})])
    (spit (str (fs/path dir "workspace.yaml")) (str "id: " csid "\ncwd: " proj "\nclient_name: copilot-intellij\nname: v3 please\n")))
  (let [dir (fs/path copilot "session-state" "nolog-1")]
    (fs/create-dirs dir)
    (spit (str (fs/path dir "workspace.yaml")) "id: nolog-1\ncwd: /elsewhere/y\nclient_name: github/cli\n")))

(defn- tree
  "{path [size mtime content-hash]} of every file under dir."
  [dir]
  (into (sorted-map) (for [f (fs/glob dir "**" {:hidden true}) :when (fs/regular-file? f)]
                       [(str f) [(fs/size f) (str (fs/last-modified-time f)) (hash (slurp (str f)))]])))

(use-fixtures :each
  (fn [t]
    (binding [found/*claude-dir* claude found/*copilot-home* copilot found/*var-dir* (str (fs/path scratch "var"))
              found/*replays* replays install/*settings* settings g/*repo* (str (fs/path scratch "var" "traces"))
              rp/*history-dir* (str (fs/path claude "file-history"))
              health/*claude-dir* claude health/*seen-dir* (str (fs/path scratch "seen"))]
      (t))))

(defn- get* [uri & {:keys [host] :or {host "127.0.0.1:7350"}}]
  (let [[path q] (str/split uri #"\?" 2)]
    (dash/handler {:uri path :query-string q :request-method :get :headers {"host" host}})))

(defn- post* [uri body & {:keys [token host] :or {host "127.0.0.1:7350"}}]
  (dash/handler {:uri uri :request-method :post :body (java.io.ByteArrayInputStream. (.getBytes (json/generate-string body)))
                 :headers (cond-> {"host" host} token (assoc "x-reus-token" token))}))

(defn- token [] (:token @@#'dash/state))

(deftest looking-writes-nothing
  (machine!)
  (swap! @#'dash/state assoc :token "tok-0")
  (let [before (tree scratch)
        c (found/claude) cp (found/copilot) hk (found/hooks)]
    (testing "Claude"
      (is (= 2 (:sessions c)) "a main transcript per project, the agent run apart")
      (is (= 1 (:agents c)))
      (is (= [proj "/elsewhere/x"] (map :cwd (sort-by :oldest (:projects c)))) "the cwd from the transcript")
      (is (= [(str sid ".jsonl") "e1.jsonl"] (sort (map #(str (fs/file-name %)) (found/claude-files (map :key (:projects c)) claude)))))
      (is (empty? (found/claude-files ["../../etc"] claude)) "a key that is no project dir picks nothing"))
    (testing "Copilot"
      (is (= 2 (count (:sessions cp))))
      (is (= 1 (:logs cp)))
      (is (= {"IntelliJ" 1 "CLI" 1} (:by-client cp)))
      (is (= "v3 please" (:name (first (filter :log (:sessions cp)))))))
    (testing "hooks: off, the file there"
      (is (= {:on? false :exists? true} (select-keys (:claude hk) [:on? :exists?])))
      (is (false? (:on? (:copilot hk)))))
    (testing "the page"
      (let [r (get* "/")]
        (is (= 200 (:status r)))
        (is (str/includes? (:body r) "Found 2 Claude sessions in 2 projects, 2 Copilot sessions. Hooks: Claude not installed, Copilot not installed. Live trace: none yet."))
        (is (str/includes? (:body r) "content=\"tok-0\"") "the token is in the page")
        (is (str/includes? (:body r) "Seven hook entries"))
        (is (= 403 (:status (get* "/" :host "evil.example:7350"))) "another Host (DNS rebinding) gets nothing")))
    (is (= before (tree scratch)) "not one file made, changed or touched")))

(deftest posts-need-this-runs-token
  (machine!)
  (swap! @#'dash/state assoc :token "tok-1")
  (is (= 403 (:status (post* "/x/hooks" {"which" "claude" "to" "on"}))) "no token")
  (is (= 403 (:status (post* "/x/hooks" {"which" "claude" "to" "on"} :token "guess"))) "a wrong one")
  (is (= 403 (:status (post* "/x/hooks" {"which" "claude" "to" "on"} :token "tok-1" :host "evil.example"))) "another Host")
  (is (= 403 (:status (post* "/x/build" {"source" "claude" "keys" ["x"]}))))
  (is (= 400 (:status (post* "/x/build" {"source" "nope" "keys" ["x"]} :token "tok-1"))))
  (is (= 400 (:status (post* "/x/build" {"source" "claude" "keys" []} :token "tok-1"))))
  (is (= (json/parse-string (json/generate-string other-settings)) (json/parse-string (slurp settings))) "nothing written"))

(deftest build-from-old-logs
  (machine!)
  (let [target (:claude-replay replays)
        key (str/replace proj "/" "-")
        seen (atom [])]
    (fs/create-dirs (fs/path target ".git"))
    (spit (str (fs/path target "old-marker")) "the old replay")
    (testing "a Claude build"
      (add-watch @#'dash/state ::w (fn [_ _ _ n] (when-let [s (get-in n [:job :state])] (swap! seen conj s))))
      (let [j (dash/build! :claude [key])]
        (remove-watch @#'dash/state ::w)
        (is (= "done" (:state j)) (str (:error j)))
        (is (= ["reading" "recording" "swapping" "done"] (distinct @seen)) "the progress went through each part")
        (is (= [1 1] [(:files j) (:read j)]))
        (is (pos? (:events j)))
        (is (= (:events j) (:done j)))
        (is (str/includes? (:summary j) "1 prompt, 2 tool calls, 1 agent run recorded") (:summary j))
        (is (not (fs/exists? (fs/path target "old-marker"))) "the old repo was replaced")
        (is (not (fs/exists? (str target ".building"))) "nothing left half made")
        (binding [g/*repo* target]
          (is (rd/central? ) "a replay browses like the live repo")
          (is (some #{"proj/s/d0d0cafe"} (keys (rd/tips)))))
        (let [r (found/repo target)
              s (first (:sessions (first (:projects r))))]
          (is (= ["proj"] (map :name (:projects r))))
          (is (= 1 (:agents r)))
          (is (= "d0d0cafe@00" (:id s)) "the viewer's id of the session's newest prompt")
          (is (str/includes? (str (:prompt s)) "make it v2")))))
    (testing "a Copilot build"
      (let [j (dash/build! :copilot [csid "not-there"])]
        (is (= "done" (:state j)) (str (:error j)))
        (is (= 1 (:files j)) "only the session that has a log")
        (is (str/includes? (:summary j) "1 prompt, 1 tool call recorded") (:summary j))
        (is (some #{"proj/s/c0c0beef"} (keys (binding [g/*repo* (:copilot-replay replays)] (rd/tips)))))))
    (testing "nothing to build from"
      (is (= "failed" (:state (dash/build! :claude ["no-such-dir"])))))
    (testing "the page links the sessions to the viewer"
      (let [body (:body (get* "/"))]
        (is (str/includes? body "/browse?r=claude-replay&amp;m=proj&amp;p=d0d0cafe%4000"))
        (is (str/includes? body "/browse?r=copilot-replay&amp;m=proj"))))
    (testing "the viewer of a built repo, started on /browse"
      (swap! @#'dash/state assoc :port 7390)
      (let [r (get* "/browse?r=claude-replay&m=proj&p=d0d0cafe@00")
            loc (get-in r [:headers "location"])]
        (try
          (is (= 302 (:status r)))
          (is (re-matches #"http://127\.0\.0\.1:\d+/p/d0d0cafe%4000\?m=proj" (str loc)) loc)
          (let [html (slurp loc)]
            (is (str/includes? html "make it v2") "the prompt")
            (is (str/includes? html "http://127.0.0.1:7390/") "the link back to the dashboard"))
          (is (= 404 (:status (get* "/browse?r=../../etc"))) "only the repos it knows")
          (finally
            (doseq [{:keys [stop]} (vals (:viewers @@#'dash/state))] (stop))
            (swap! @#'dash/state assoc :viewers {})))))))

(deftest hooks-on-and-off
  (machine!)
  (let [before (json/parse-string (slurp settings))]
    (testing "what on would write, before it does"
      (let [{:keys [after]} (install/plan :on (str (fs/path g/home "bb.edn")))]
        (is (str/includes? after "-m trace.hook prompt"))
        (is (str/includes? after "say done") "the other hook stays")
        (is (= before (json/parse-string (slurp settings))) "plan writes nothing")))
    (testing "Claude"
      (dash/switch-hooks! :claude :on)
      (let [st (install/status)]
        (is (:on? st))
        (is (= #{g/home} (:checkouts st)) "the hook runs this checkout")
        (is (= 7 (count (:events st)))))
      (is (str/includes? (:body (get* "/")) "Hooks: Claude on"))
      (is (= before (json/parse-string (slurp (str settings ".bak")))) "the old one kept")
      (dash/switch-hooks! :claude :off)
      (is (false? (:on? (install/status))))
      (is (= before (json/parse-string (slurp settings))) "off leaves the settings as they were"))
    (testing "a machine with no settings.json yet"
      (fs/delete settings)
      (dash/switch-hooks! :claude :on)
      (is (:on? (install/status)))
      (dash/switch-hooks! :claude :off)
      (is (= {"hooks" {}} (json/parse-string (slurp settings)))))
    (testing "Copilot"
      (dash/switch-hooks! :copilot :on)
      (let [st (ci/status copilot)]
        (is (:on? st))
        (is (:skill? st))
        (is (= #{g/home} (:checkouts st))))
      (is (str/includes? (:body (get* "/")) "Copilot on"))
      (dash/switch-hooks! :copilot :off)
      (is (= {:on? false :skill? false} (select-keys (ci/status copilot) [:on? :skill?]))))))
