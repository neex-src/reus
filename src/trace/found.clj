(ns trace.found
  "What is on this machine, for bb gui (trace.dash) to show. READ ONLY: nothing here writes a file, makes a dir,
  takes a lock or runs a git command that writes. (A test runs all of it over a made-up machine and compares
  every file before and after.)

    (claude)       Claude Code's transcripts ($CLAUDE_CONFIG_DIR, else ~/.claude: projects/<dir>/<session>.jsonl,
                   <session>/subagents/agent-*.jsonl): per project dir its cwd (from a transcript's head),
                   sessions, agent runs, bytes, newest and oldest write
    (copilot)      Copilot's sessions ($COPILOT_HOME, else ~/.copilot: session-state/<id>/workspace.yaml and
                   events.jsonl): cwd, client (the CLI or the IntelliJ plugin), which have a log to replay
    (hooks)        are the reus hooks in place, and which checkout they run (trace.install, trace.copilot-install)
    (repos)        the trace repos: the live one (trace.git/*repo*) and every repo in <reus>/var, per project its
                   sessions (branch, prompts, newest prompt's text), agent runs, newest commit
    (workers)      trace.worker processes running now (ps)
    (health)       trace.health/problems of the live repo
    (port-open? p) is something listening on 127.0.0.1:p (a bb trace:gui started by hand)"
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [jsonl.read :as jr]
            [trace.copilot :as cop]
            [trace.copilot-install :as ci]
            [trace.copilot-replay :as cr]
            [trace.git :as g]
            [trace.health :as health]
            [trace.install :as install]
            [trace.replay :as rp]))

(def ^:dynamic *claude-dir* "Claude Code's dir." jr/claude-dir)
(def ^:dynamic *copilot-home* "Copilot's home; nil: trace.copilot/copilot-home." nil)
(def ^:dynamic *var-dir* "Where this checkout keeps its trace repos." (str (fs/path g/home "var")))
(def ^:dynamic *replays*
  "The repos the build buttons write: {:claude-replay dir :copilot-replay dir}."
  {:claude-replay rp/default-repo :copilot-replay cr/default-repo})

(defn- mtime [f] (.toMillis (fs/last-modified-time f)))

(defn- head-cwd
  "The cwd a transcript names in its first 64 KB, nil when none."
  [f]
  (try
    (with-open [in (java.io.FileInputStream. (str f))]
      (let [buf (byte-array 65536) n (.read in buf)]
        (when (pos? n) (some-> (re-find #"\"cwd\":\"([^\"]+)\"" (String. buf 0 n "UTF-8")) second))))
    (catch Exception _ nil)))

(defn- decoded
  "A project dir's name as the path it stands for (Claude Code turns / and . into -: a guess, for a dir whose
  transcripts name no cwd)."
  [k]
  (str/replace k "-" "/"))

(defn- jsonls [^java.io.File dir prefix]
  (filter #(let [n (.getName ^java.io.File %)] (and (str/ends-with? n ".jsonl") (str/starts-with? n prefix) (.isFile ^java.io.File %)))
          (or (.listFiles dir) [])))

(defn claude-project
  "One project dir of Claude Code: {:key :dir :cwd :name :sessions :agents :bytes :newest :oldest}, nil when it
  has no session. Only the dir and each session's subagents/ are listed (a session dir holds much more)."
  [d]
  (let [d (fs/file d)
        mains (sort-by #(- (.lastModified ^java.io.File %)) (jsonls d ""))]
    (when (seq mains)
      (let [agents (mapcat #(jsonls (java.io.File. ^java.io.File % "subagents") "agent-")
                           (filter #(.isDirectory ^java.io.File %) (.listFiles d)))
            all (concat mains agents)
            times (map #(.lastModified ^java.io.File %) all)
            cwd (or (some head-cwd (take 3 mains)) (decoded (.getName d)))]
        {:key (.getName d) :dir (str d) :cwd cwd :name (str (fs/file-name cwd))
         :sessions (count mains) :agents (count agents)
         :bytes (reduce + (map #(.length ^java.io.File %) all)) :newest (apply max times) :oldest (apply min times)}))))

(defn claude
  "Claude Code's transcripts under dir (default *claude-dir*): {:dir :exists? :projects [newest first]
  :sessions :agents :bytes :newest}."
  [& [dir]]
  (let [dir (str (or dir *claude-dir*))
        pdir (fs/path dir "projects")
        ps (if (fs/directory? pdir)
             (->> (fs/list-dir pdir) (filter fs/directory?) (pmap claude-project) (remove nil?) (sort-by :newest >) vec)
             [])]
    {:dir dir :exists? (fs/directory? dir) :projects ps
     :sessions (reduce + (map :sessions ps)) :agents (reduce + (map :agents ps))
     :bytes (reduce + (map :bytes ps)) :newest (some->> (seq ps) (map :newest) (apply max))}))

(defn claude-files
  "The main transcripts of the project dirs keys (of (claude dir)), oldest name first; keys it does not have
  are left out."
  [keys & [dir]]
  (let [pdir (fs/path (or dir *claude-dir*) "projects")
        known (set (map #(str (fs/file-name %)) (filter fs/directory? (if (fs/directory? pdir) (fs/list-dir pdir) []))))]
    (sort (for [k keys :when (known k) f (fs/glob (fs/path pdir k) "*.jsonl")] (str f)))))

(def clients {"github/cli" "CLI" "copilot-intellij" "IntelliJ" "vscode" "VS Code"})

(defn copilot
  "Copilot's sessions under home (default the Copilot home): {:home :exists? :sessions [newest first] :logs n
  :by-client {client n}}; a session {:id :dir :cwd :client :name :created :updated :log (its events.jsonl or
  nil) :bytes}."
  [& [home]]
  (let [home (str (or home *copilot-home* (cop/copilot-home)))
        st (fs/path home "session-state")
        ss (if (fs/directory? st)
             (->> (fs/list-dir st)
                  (filter fs/directory?)
                  (map (fn [d]
                         (let [w (cr/workspace d)
                               log (fs/path d "events.jsonl")
                               log? (fs/regular-file? log)
                               client (get w "client_name")]
                           {:id (str (fs/file-name d)) :dir (str d) :cwd (get w "cwd")
                            :client (get clients client (or client "?")) :name (get w "name")
                            :created (get w "created_at")
                            :updated (if log? (mtime log) (mtime d))
                            :log (when log? (str log)) :bytes (if log? (fs/size log) 0)})))
                  (sort-by :updated >)
                  vec)
             [])]
    {:home home :exists? (fs/directory? home) :sessions ss
     :logs (count (filter :log ss)) :by-client (frequencies (map :client ss))}))

(defn hooks
  "{:claude (trace.install/status) :copilot (trace.copilot-install/status) :here this checkout}."
  []
  {:claude (try (install/status) (catch Throwable e {:error (ex-message e)}))
   :copilot (try (ci/status *copilot-home*) (catch Throwable e {:error (ex-message e)}))
   :here g/home})

;; --- trace repos --------------------------------------------------------------------------------

(defn- refs
  "[[branch unix-time subject]] of a repo's branches, read only."
  [dir]
  (let [{:keys [exit out]} (p/shell {:dir (str dir) :out :string :err :string :continue true}
                                    "git" "for-each-ref" "--format=%(refname:short)%09%(committerdate:unix)%09%(objectname)%09%(subject)" "refs/heads")]
    (when (zero? exit)
      (for [l (str/split-lines out) :let [[b t sha s] (str/split l #"\t" 4)] :when sha]
        [b (some-> t parse-long) sha (str s)]))))

(defn- prompt-texts
  "{sha text} of the newest prompt in each commit's tree (.claude-trace/prompt.md), one cat-file process."
  [dir shas]
  (binding [g/*repo* (str dir)]
    (into {} (map (fn [[rev t]] [(first (str/split rev #":")) t]))
          (g/cat-batch (map #(str % ":.claude-trace/prompt.md") shas)))))

(defn repo
  "A trace repo: {:dir :name :layout (:record a trace.record repo, :bench a trace.bb one) :projects [newest
  first] :sessions :agents :newest}; a project {:name :sessions [{:branch :id :nn :prompts :at :prompt}] :agents n
  :newest}. :id is the viewer's prompt id (<session8>@NN) of the session's newest prompt; nil when dir is no
  git repo."
  [dir]
  (when (fs/exists? (fs/path dir ".git"))
    (let [rs (refs dir)
          sess (keep (fn [[b t sha s]]
                       (when-let [[_ proj kind suf] (re-matches #"(.+)/([sa])/(.+)" b)]
                         (let [nn (second (re-find #"^(?:prompt |response )?(\d+)" s))]
                           {:project proj :agent? (= "a" kind) :branch b :suffix suf :at t :sha sha
                            :nn nn :prompts (some-> nn parse-long inc)})))
                     rs)
          mains (set (keep #(second (re-matches #"(.+)/main" (first %))) rs))
          texts (prompt-texts dir (map :sha (remove :agent? sess)))
          projects (->> (group-by :project sess)
                        (map (fn [[proj ss]]
                               (let [ss (sort-by :at > ss) own (remove :agent? ss)]
                                 {:name proj
                                  :sessions (vec (for [s own]
                                                   (assoc (select-keys s [:branch :at :prompts :nn])
                                                          :id (when (:nn s) (str (:suffix s) "@" (:nn s)))
                                                          :prompt (get texts (:sha s)))))
                                  :agents (count (filter :agent? ss))
                                  :newest (:at (first ss))})))
                        (sort-by #(- (or (:newest %) 0)))
                        vec)]
      {:dir (str dir) :name (str (fs/file-name dir))
       :layout (if (or (seq sess) (fs/exists? (fs/path dir ".git" "trace-sessions"))) :record :bench)
       :mains (count mains)
       :projects projects
       :sessions (reduce + (map (comp count :sessions) projects))
       :agents (reduce + (map :agents projects))
       :newest (some->> (seq (keep :at sess)) (apply max))})))

(defn repo-dirs
  "[[kind dir]] of the trace repos this checkout knows: :live (trace.git/*repo*), :claude-replay and
  :copilot-replay (*replays*, there or not), and every other git repo in *var-dir* (<reus>/var)."
  []
  (let [named [[:live (str g/*repo*)] [:claude-replay (:claude-replay *replays*)] [:copilot-replay (:copilot-replay *replays*)]]
        seen (set (map (comp str fs/absolutize second) named))
        var *var-dir*
        others (when (fs/directory? var)
                 (->> (fs/list-dir var)
                      (filter #(and (fs/directory? %) (fs/exists? (fs/path % ".git"))))
                      (map str) (remove (comp seen str fs/absolutize)) sort))]
    (vec (concat named (map #(vector :other %) others)))))

(defn repos
  "{kind-or-dir info}: (repo dir) for each of repo-dirs, keyed by its kind (the others by their dir); an
  absent one has {:dir :absent? true}."
  []
  (into {} (for [[k d] (repo-dirs)] [(if (= :other k) d k) (or (repo d) {:dir d :name (str (fs/file-name d)) :absent? true})])))

;; --- running ------------------------------------------------------------------------------------

(defn workers
  "[{:pid :checkout}] of the trace.worker processes running now (the hook starts one per trace repo when
  events come; it exits when idle)."
  []
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true} "ps" "-axo" "pid=,args=")]
    (when (zero? exit)
      (vec (for [l (str/split-lines out)
                 :let [[_ pid args] (re-matches #"\s*(\d+)\s+(.*)" l)]
                 :when (and args (str/includes? args "-m trace.worker"))]
             {:pid (parse-long pid)
              :checkout (some-> (re-find #"--config\s+(\S+)/bb\.edn" args) second)})))))

(defn worker-log
  "When the live repo's worker last wrote its log (ms), nil without one."
  []
  (let [f (fs/path g/*repo* ".git" "trace-worker.log")] (when (fs/exists? f) (mtime f))))

(defn health
  "trace.health/problems of the live repo as [[label since reason]]; [] when ok; nil without a live repo."
  []
  (when (fs/exists? (fs/path g/*repo* ".git"))
    (try (health/problems) (catch Throwable e [["health unreadable" nil (ex-message e)]]))))

(defn port-open?
  "Does something listen on 127.0.0.1:port?"
  [port]
  (try (with-open [s (java.net.Socket.)]
         (.connect s (java.net.InetSocketAddress. "127.0.0.1" (int port)) 200)
         true)
       (catch Exception _ false)))
