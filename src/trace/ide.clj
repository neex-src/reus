(ns trace.ide
  "Run prompts through the engine of the GitHub Copilot plugin of IntelliJ, without the IDE: the plugin's own
  `copilot-language-server` (LSP over stdio) is started the way the plugin starts it and driven with the
  plugin's `backgroundAgent/*` requests (create, send, the session events, the permission answers). The
  session is the plugin's \"Copilot\" agent: Copilot's runtime, writing <copilot home>/session-state/<id>/events.jsonl
  with workspace.yaml client_name `copilot-intellij`, calling the user-level hooks (<copilot home>/hooks/*.json).
  This is how `bb runs:go --arm ide:MODEL[:EFFORT]` compares the CLI with the IDE on the same task.

  Needs the plugin installed (the server binary is found under ~/.local/share/JetBrains/*/github-copilot-intellij,
  or $COPILOT_LANGUAGE_SERVER) and signed in once in the IDE. Nothing of the IDE's is touched: the server runs
  with a home of its own, var/ide/ in this checkout (HOME, COPILOT_HOME, XDG_*), seeded with a copy of the
  sign-in (~/.config/github-copilot: auth.db, oauth.json) the first time. Its COPILOT_HOME gets reus' hook file
  (trace.copilot-install), so every run is recorded by trace.copilot like any other Copilot session.

  Models: the id as the account lists it (`backgroundAgent/listModels`; Copilot plans often list only `auto`),
  or a part of it (`haiku`). A model the account does not list is run through a BYOK provider when
  a key is there ($ANTHROPIC_API_KEY, else a gitignored file: see *key-files*) (`haiku` = claude-haiku-5-5, `claude-*` as given): the plugin's own custom-endpoint
  feature (Anthropic Messages API, no Copilot request is spent). The agent runs in autopilot mode with every
  permission request approved (what `copilot -p --allow-all-tools` does), so it ends each prompt with a
  `task_complete` call the CLI does not make."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [trace.copilot-install :as install]
            [trace.git :as g]))

(defn ide-dir [] (str (fs/path g/home "var" "ide")))

(defn server-bin
  "The plugin's language server: $COPILOT_LANGUAGE_SERVER, else the newest under the JetBrains user plugin dirs."
  []
  (or (System/getenv "COPILOT_LANGUAGE_SERVER")
      (->> (fs/glob (fs/path (fs/home) ".local" "share" "JetBrains") "*/github-copilot-intellij/copilot-agent/native/*/copilot-language-server")
           (sort-by fs/last-modified-time) last str not-empty)
      (throw (ex-info "the GitHub Copilot plugin of IntelliJ is not installed (no copilot-language-server found); set $COPILOT_LANGUAGE_SERVER" {}))))

(defn- seed-auth!
  "Copy the IDE's sign-in into the server's own config home, once."
  [xdgc]
  (let [dst (fs/path xdgc "github-copilot") src (fs/path (fs/home) ".config" "github-copilot")]
    (when-not (fs/exists? (fs/path dst "oauth.json"))
      (fs/create-dirs dst)
      (doseq [f ["auth.db" "auth.db-wal" "auth.db-shm" "oauth.json" "versions.json"]
              :when (fs/exists? (fs/path src f))]
        (fs/copy (fs/path src f) (fs/path dst f) {:replace-existing true})))))

(def ^:dynamic *key-files*
  "Where the BYOK key is looked for when no environment variable has it, in order: $REUS_IDE_BYOK_FILE, var/byok.env
  of the main checkout (gitignored), ~/projects/copilot-lab/.env. KEY=value lines (`export ` and quotes allowed)."
  nil)

(defn- key-files []
  (or *key-files*
      (remove nil? [(System/getenv "REUS_IDE_BYOK_FILE")
                    (str (fs/path (g/main-checkout) "var" "byok.env"))
                    (str (fs/path (fs/home) "projects" "copilot-lab" ".env"))])))

(defn- key-in-file
  "The value of REUS_IDE_BYOK_KEY or ANTHROPIC_API_KEY in an env-style file, nil when the file or the line is not there."
  [file]
  (when (fs/regular-file? file)
    (let [vals (into {} (keep #(when-let [[_ k v] (re-matches #"\s*(?:export\s+)?([A-Z_]+)\s*=\s*(.*?)\s*" %)]
                                 [k (str/replace v #"^(['\"])(.*)\1$" "$2")]))
                      (str/split-lines (slurp (str file))))]
      (not-empty (or (get vals "REUS_IDE_BYOK_KEY") (get vals "ANTHROPIC_API_KEY"))))))

(defn byok-key
  "The BYOK key: $REUS_IDE_BYOK_KEY or $ANTHROPIC_API_KEY, else the first of key-files that has one. Never printed."
  []
  (or (not-empty (System/getenv "REUS_IDE_BYOK_KEY")) (not-empty (System/getenv "ANTHROPIC_API_KEY"))
      (some key-in-file (key-files))))

(defn server-env
  "The environment of the server: everything it writes lands in var/ide."
  []
  (let [d (ide-dir)
        e {"HOME" (str (fs/path d "home")) "COPILOT_HOME" (str (fs/path d "home" ".copilot"))
           "XDG_CONFIG_HOME" (str (fs/path d "xdgc")) "XDG_STATE_HOME" (str (fs/path d "xdgs")) "XDG_DATA_HOME" (str (fs/path d "xdgd"))
           "XDG_CACHE_HOME" (str (fs/path d "xdgcache")) "TMPDIR" (str (fs/path d "tmp")) "PATH" (System/getenv "PATH") "NODE_ICU_DATA" ""
           "REUS_HOME" g/home "TRACE_REPO" (str g/*repo*)}]
    (doseq [[k v] e :when (str/starts-with? k "XDG_")] (fs/create-dirs v))
    (fs/create-dirs (get e "TMPDIR")) (fs/create-dirs (get e "COPILOT_HOME"))
    (seed-auth! (get e "XDG_CONFIG_HOME"))
    (install/spit-hooks! (fs/path (get e "COPILOT_HOME") "hooks" "reus.json"))
    e))

;; --- LSP over stdio ------------------------------------------------------------------------

(defn- read-msg [^java.io.InputStream out]
  (loop [hdr ""]
    (let [c (.read out)]
      (cond (neg? c) nil
            (str/ends-with? (str hdr (char c)) "\r\n\r\n")
            (let [n (Integer/parseInt (second (re-find #"Content-Length: (\d+)" (str hdr (char c)))))
                  buf (byte-array n)]
              (loop [o 0] (when (< o n) (let [r (.read out buf o (- n o))] (when (pos? r) (recur (+ o r))))))
              (json/parse-string (String. buf "UTF-8")))
            :else (recur (str hdr (char c)))))))

(defn start!
  "Start the server for workspace folder `dir` and initialize it as the plugin does. A client map; (stop! c) ends it."
  [dir]
  (let [env (server-env)
        proc (p/process [(server-bin) "--stdio"] {:env env :dir dir :err (io/file (ide-dir) "server.err") :inherit false})
        out (io/input-stream (:out proc)) in (:in proc)
        pending (atom {}) idc (atom 0) idle (atom (promise)) lock (Object.)
        send! (fn [m] (let [b (.getBytes (json/generate-string (merge {:jsonrpc "2.0"} m)) "UTF-8")]
                        (locking lock
                          (.write in (.getBytes (str "Content-Length: " (count b) "\r\n\r\n"))) (.write in b) (.flush in))))
        call (fn [method params & [ms]]
               (let [id (swap! idc inc) pr (promise)]
                 (swap! pending assoc id pr)
                 (send! {:id id :method method :params params})
                 (deref pr (or ms 30000) {"error" {"message" (str "timeout: " method)}})))
        reader (future
                 (loop []
                   (when-let [m (read-msg out)]
                     (let [method (get m "method") id (get m "id") ev (get-in m ["params" "event"])]
                       (cond
                         (and method id) (send! {:id id :result nil}) ; the IDE's side of the server's own requests
                         (and (= "backgroundAgent/sessionUpdate" method) (= "permission.requested" (get ev "type")))
                         (send! {:id (+ 100000 (swap! idc inc)) :method "backgroundAgent/interaction"
                                 :params {:sessionId (get-in m ["params" "sessionId"]) :agentProvider "BACKGROUND"
                                          :requestId (get-in ev ["data" "requestId"]) :type "permission" :result {:kind "approve-once"}}})
                         (and (= "backgroundAgent/sessionUpdate" method) (= "session.idle" (get ev "type"))) (deliver @idle true)
                         (and id (not method)) (when-let [pr (@pending id)] (deliver pr m))))
                     (recur))))
        ws (str "file://" dir)
        r (call "initialize" {:processId nil :rootUri ws :workspaceFolders [{:uri ws :name (str (fs/file-name dir))}]
                              :clientInfo {:name "JetBrains-IU" :version "2026.2.3"}
                              :initializationOptions {:editorInfo {:name "JetBrains-IU" :version "2026.2.3"}
                                                      :editorPluginInfo {:name "copilot-intellij" :version "1.20.0-261"}}
                              :capabilities {:workspace {:workspaceFolders true :configuration true}}} 60000)]
    (when (get r "error") (p/destroy-tree proc) (throw (ex-info (str "the Copilot language server did not start: " (get-in r ["error" "message"])) {})))
    (send! {:method "initialized" :params {}})
    {:proc proc :call call :idle idle :reader reader :ws dir}))

(defn stop! [{:keys [proc]}] (p/destroy-tree proc))

(defn- ok! [what r]
  (when-let [e (get r "error")] (throw (ex-info (str what ": " (get e "message")) {:response r})))
  (get r "result"))

;; --- models ----------------------------------------------------------------------------------

(defn choose-model
  "[model-id byok?] for the arm's model name, given the ids the account lists; nil when it can be neither listed nor run by BYOK."
  [ids want byok?]
  (let [want (str/lower-case (str want))]
    (cond (some #{want} ids) [want false]
          (some #(str/includes? (str/lower-case %) want) ids) [(first (filter #(str/includes? (str/lower-case %) want) ids)) false]
          byok? [(if (str/starts-with? want "claude-") want (get {"haiku" "claude-haiku-5-5"} want want)) true])))

(defn- byok-provider! [{:keys [call]} model]
  (let [k (byok-key)]
    (ok! "save provider" (call "copilot/byok/saveCustomProviderConfig" {:providerName "reus" :apiKey k :groupName "reus" :apiType "messages"}))
    (ok! "save model" (call "copilot/byok/saveModel"
                            {:providerName "reus" :modelId model :isRegistered true :isCustomModel true :apiKey k
                             :deploymentUrl "https://api.anthropic.com/v1/messages"
                             :modelCapabilities {:name model :maxInputTokens 200000 :maxOutputTokens 32000 :toolCalling true :vision false :thinking false}}))))

(defn resolve-model!
  "{:model :provider} to create/send with, for the arm's model name (BYOK provider registered when needed); throws with the listed ids when none fits."
  [{:keys [call] :as c} want]
  (let [ids (mapv #(get % "id") (get (ok! "list models" (call "backgroundAgent/listModels" {:agentProvider "BACKGROUND" :forceRefresh true} 60000)) "models"))
        [m byok?] (choose-model ids want (boolean (byok-key)))]
    (cond (nil? m) (throw (ex-info (format "model %s: the account lists %s, and $ANTHROPIC_API_KEY is not set for a BYOK run" want (pr-str ids)) {}))
          byok? (do (byok-provider! c m) {:model m :provider "reus"})
          :else {:model m})))

;; --- one session -----------------------------------------------------------------------------

(def ^:dynamic *prompt-timeout-ms* "How long one prompt may take." (* 20 60 1000))

(defn run-session!
  "Run `prompts` one after the other as ONE session of the plugin's Copilot agent in dir (a git repo), model and effort
  as named. {:session :ok? :prompts n}; calls (say line) per prompt."
  [{:keys [dir session model effort prompts say] :or {say println}}]
  (let [dir (str (fs/absolutize dir))
        c (start! dir)]
    (try
      (let [{:keys [call idle]} c
            st (ok! "check status" (call "checkStatus" {}))
            _ (when-not (= "OK" (get st "status")) (throw (ex-info (str "Copilot is not signed in (" (get st "status") "): sign in once in IntelliJ") {})))
            {:keys [model provider]} (resolve-model! c model)
            sid (or session (str (random-uuid)))
            _ (ok! "create session" (call "backgroundAgent/create"
                                          (cond-> {:sessionId sid :agentProvider "BACKGROUND" :model model :workspaceFolder {:uri (str "file://" dir) :name (str (fs/file-name dir))}}
                                            provider (assoc :modelProvider provider) effort (assoc :reasoningEffort effort)) 60000))]
        (loop [[prompt & more] prompts n 1]
          (if-not prompt
            {:session sid :ok? true :prompts (dec n)}
            (let [t0 (System/currentTimeMillis)
                  _ (reset! idle (promise))
                  s (call "backgroundAgent/send"
                          (cond-> {:sessionId sid :agentProvider "BACKGROUND" :message prompt :model model :agentMode "autopilot"}
                            provider (assoc :modelProvider provider) effort (assoc :reasoningEffort effort)) 120000)
                  done (when-not (get s "error") (deref @idle *prompt-timeout-ms* ::timeout))]
              (say (format "prompt %d/%d %s, %.0fs" n (count prompts) (cond (get s "error") "failed" (= ::timeout done) "timed out" :else "done")
                           (/ (- (System/currentTimeMillis) t0) 1000.0)))
              (if (or (get s "error") (= ::timeout done))
                {:session sid :ok? false :prompts (dec n) :error (or (get-in s ["error" "message"]) "timeout")}
                (do (Thread/sleep 1500) ; the last hooks (stop, session end) run just after the idle event
                    (recur more (inc n))))))))
      (finally (stop! c)))))
