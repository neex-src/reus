(ns trace.runner
  "Run the same prompts against several models / reasoning efforts, and make the comparison (trace.runs).

    bb runs:go  --task NAME --prompts DIR|FILE [--start DIR] [--check CMD] --arm [name=]MODEL[:EFFORT] ...
    bb runs:new                 the same, asking for each thing (a wizard; stdin)
    bb runs:html TASK [FILE]    the comparison as one static HTML file

  A TASK is var/tasks/NAME/ in this reus checkout: start/ (a snapshot of the folder to work in: the files git
  knows, else everything but .git and node_modules; plus `.reus-check` when --check is given), prompts/NN.md
  (--prompts: a folder of .md/.txt files in name order, or one file whose prompts are separated by a line of
  `---`), task.edn. An ARM is one model/effort. Each arm gets a fresh copy of start/ in var/runs-work/NAME--ARM
  (a git repo with one commit, so reus names the project `NAME--ARM`) and runs the prompts one after the other
  as ONE session (an arm `ide:MODEL[:EFFORT]` runs it through the engine of IntelliJ's Copilot plugin instead, headless: trace.ide): `copilot -p PROMPT --allow-all-tools --session-id UUID [--model M] [--reasoning-effort E]`, the
  first call making the session and the others resuming it. Arms run side by side (--serial: one after the
  other). A second run of the same arm replaces the copy and is one more session of the same project.

  Recording is reus's own: the hooks in the user's Copilot home (bb copilot:install). When they are not there
  the arm gets the same hooks as a repo-level file (.github/hooks/reus.json), which Copilot reads for a trusted
  folder (--trust-folder). The copilot to run: $COPILOT_BIN, else `copilot` on the PATH (a `.bb` file is run
  with bb). When all arms are done the worker is waited for and the static HTML is written to
  var/reports/NAME.html."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [trace.git :as g]
            [trace.copilot-install :as install]
            [trace.runs :as runs]))

(defn- var-dir [& parts] (str (apply fs/path g/home "var" parts)))
(defn task-dir [task] (var-dir "tasks" task))

(defn- sh [dir & args] (apply p/shell {:dir (str dir) :out :string :err :string :continue true} args))

(defn- slug [s] (str/replace (str/lower-case (str s)) #"[^a-z0-9._]+" "-"))

;; --- the task ------------------------------------------------------------------------------

(def ^:private max-bytes (* 100 1024 1024))

(defn- start-files
  "Relative paths of what a snapshot of dir holds: git's tracked and untracked-not-ignored files, else all but .git / node_modules."
  [dir]
  (let [r (sh dir "git" "ls-files" "-z" "--cached" "--others" "--exclude-standard")
        listed (when (zero? (:exit r)) (filter #(fs/regular-file? (fs/path dir %)) (remove str/blank? (str/split (:out r) #"\u0000"))))]
    (if (seq listed) ; a folder git knows nothing about (or inside an ignored dir of another repo) lists nothing: walk it
      listed
      (->> (fs/glob dir "**" {:hidden true})
           (map #(str (fs/relativize dir %)))
           (remove #(re-find #"(^|/)(\.git|node_modules)(/|$)" %))
           (filter #(fs/regular-file? (fs/path dir %)))))))

(defn snapshot!
  "Copy the start files of dir into dest."
  [dir dest]
  (let [fs* (vec (start-files dir))
        total (reduce + 0 (map #(fs/size (fs/path dir %)) fs*))]
    (when (> total max-bytes)
      (throw (ex-info (str dir " is " (quot total 1048576) " MB of files; the start folder must be a small project (limit 100 MB)") {})))
    (fs/create-dirs dest)
    (doseq [f fs*]
      (fs/create-dirs (fs/parent (fs/path dest f)))
      (fs/copy (fs/path dir f) (fs/path dest f) {:replace-existing true}))
    (count fs*)))

(defn- read-prompts
  "Prompt texts from a folder (its .md/.txt files in name order) or a file (prompts separated by a line `---`)."
  [src]
  (cond (fs/directory? src) (->> (fs/list-dir src) (filter #(re-find #"\.(md|txt)$" (str %))) sort (map (comp str/trim slurp str)))
        (fs/regular-file? src) (->> (str/split (slurp (str src)) #"(?m)^---\s*$") (map str/trim))
        :else (throw (ex-info (str "no such prompts folder or file: " src) {}))))

(defn make-task!
  "Create var/tasks/<task>: {:task :start (dir, default the current dir) :prompts (path) :check (command, optional)}."
  [{:keys [task start prompts check]}]
  (let [dir (task-dir task)
        ps (remove str/blank? (read-prompts prompts))]
    (when (empty? ps) (throw (ex-info "no prompts" {})))
    (fs/delete-tree dir)
    (fs/create-dirs (fs/path dir "prompts"))
    (let [n (snapshot! (or start ".") (fs/path dir "start"))]
      (when check (spit (str (fs/path dir "start" ".reus-check")) (str check "\n")))
      (doseq [[i t] (map-indexed vector ps)]
        (spit (str (fs/path dir "prompts" (format "%02d.md" (inc i)))) (str t "\n")))
      (spit (str (fs/path dir "task.edn"))
            (pr-str {:name task :files n :check check :prompts (mapv #(format "%02d.md" (inc %)) (range (count ps)))}))
      {:dir dir :prompts (count ps) :files n})))

;; --- the arms ------------------------------------------------------------------------------

(defn parse-arm
  "`[name=][ide:]model[:effort]` -> {:arm :model :effort :engine}; model `default` leaves the model to Copilot. With
  the `ide:` prefix the arm runs through the engine of IntelliJ's Copilot plugin (trace.ide), not the CLI."
  [s]
  (let [[_ nm rest*] (re-matches #"(?:([^=:]+)=)?(.+)" s)
        ide? (str/starts-with? rest* "ide:")
        [model effort] (str/split (cond-> rest* ide? (subs 4)) #":" 2)]
    (cond-> {:arm (slug (or nm (str (when ide? "ide-") model (when effort (str "-" effort))))) :model (when-not (= "default" model) model) :effort effort}
      ide? (assoc :engine :ide))))

(def ^:dynamic *copilot-bin* "Overrides $COPILOT_BIN (tests)." nil)

(defn copilot-cmd
  "The words that start Copilot: $COPILOT_BIN or `copilot`; a .bb file goes through bb."
  []
  (let [bin (or *copilot-bin* (System/getenv "COPILOT_BIN") "copilot")]
    (if (str/ends-with? bin ".bb") ["bb" bin] [bin])))

(defn copilot-home [] (or (System/getenv "COPILOT_HOME") (str (fs/path (fs/home) ".copilot"))))

(def ^:dynamic *copilot-home* "Overrides the Copilot home (tests)." nil)

(defn hooks-installed? [] (fs/exists? (fs/path (or *copilot-home* (copilot-home)) "hooks" "reus.json")))

(defn- git-start! [dir]
  (sh dir "git" "init" "-q" "-b" "main")
  (sh dir "git" "add" "-A")
  (sh dir "git" "-c" "user.name=reus" "-c" "user.email=reus@local" "commit" "-q" "-m" "start"))

(defn run-arm!
  "Run the task's prompts as one session in a fresh copy; calls (say line) with progress. {:arm :dir :session :ok? :prompts n :ms}."
  [task {:keys [arm model effort engine]} say]
  (let [ide? (= :ide engine)
        tdir (task-dir task)
        spec (edn/read-string (slurp (str (fs/path tdir "task.edn"))))
        dir (var-dir "runs-work" (str task "--" arm))
        logs (var-dir "runs-work" "logs")
        sid (str (java.util.UUID/randomUUID))
        own-hooks? (and (not ide?) (not (hooks-installed?)))]
    (fs/delete-tree dir)
    (fs/create-dirs logs)
    (fs/copy-tree (fs/path tdir "start") dir)
    (when own-hooks? (install/spit-hooks! (fs/path dir ".github" "hooks" "reus.json")))
    (git-start! dir)
    (say (format "%s: session %s in %s%s" arm (subs sid 0 8) dir (if own-hooks? " (repo-level hooks)" "")))
    (if ide?
      ;; the IntelliJ plugin's engine: one session, the hooks are the ones in trace.ide's own Copilot home
      (let [t00 (System/currentTimeMillis)
            r ((requiring-resolve 'trace.ide/run-session!)
               {:dir dir :session sid :model (or model "auto") :effort effort :say #(say (str arm ": " %))
                :prompts (mapv #(str/trim (slurp (str (fs/path tdir "prompts" %)))) (:prompts spec))})]
        (assoc r :arm arm :dir dir :ms (- (System/currentTimeMillis) t00)))
    (let [t00 (System/currentTimeMillis)]
      (loop [[f & more] (:prompts spec) n 1]
        (if-not f
          {:arm arm :dir dir :session sid :ok? true :prompts (dec n) :ms (- (System/currentTimeMillis) t00)}
          (let [prompt (str/trim (slurp (str (fs/path tdir "prompts" f))))
                t0 (System/currentTimeMillis)
                r (apply p/shell {:dir dir :continue true :out (str (fs/path logs (format "%s--%s-%02d.log" task arm n))) :err :out
                                  :extra-env {"REUS_HOME" g/home "TRACE_REPO" (str g/*repo*)}}
                         (concat (when (fs/which "timeout") ["timeout" "1200"])
                                 (copilot-cmd)
                                 ["-p" prompt "--allow-all-tools" "--session-id" sid]
                                 (when own-hooks? ["--trust-folder"])
                                 (when model ["--model" model])
                                 (when effort ["--reasoning-effort" effort])))]
            (say (format "%s: prompt %d/%d exit %d, %.0fs" arm n (count (:prompts spec)) (:exit r) (/ (- (System/currentTimeMillis) t0) 1000.0)))
            (if (zero? (:exit r))
              (recur more (inc n))
              {:arm arm :dir dir :session sid :ok? false :prompts (dec n) :ms (- (System/currentTimeMillis) t00)
               :log (str (fs/path logs (format "%s--%s-%02d.log" task arm n)))}))))))))

(defn- wait-recorded!
  "Let the worker finish the spool (the hooks of the last prompts), up to 2 minutes."
  []
  (try ((requiring-resolve 'trace.worker/drain!) (str g/*repo*) 120000) (catch Throwable _ nil)))

(defn go!
  "Run the task's arms (a vector of parse-arm maps), wait for the recording, write the static HTML; returns {:results :html}."
  [task arms {:keys [serial? say] :or {say println}}]
  (let [say (let [l (Object.)] (fn [s] (locking l (say s))))
        results (if serial?
                  (mapv #(run-arm! task % say) arms)
                  (mapv deref (mapv #(future (run-arm! task % say)) arms)))]
    (say "recording the last events …")
    (wait-recorded!)
    (let [out (var-dir "reports" (str task ".html"))]
      (fs/create-dirs (fs/parent out))
      (spit out (runs/static-html task))
      {:results results :html out})))

;; --- bb runs:go / runs:html / runs:new ---------------------------------------------------------

(defn- opts
  "{:flags {k [values]}} of `--k v` pairs (and bare `--serial`)."
  [args]
  (loop [[a & more] args m {}]
    (cond (nil? a) m
          (= "--serial" a) (recur more (assoc m "serial" ["true"]))
          (str/starts-with? a "--") (recur (rest more) (update m (subs a 2) (fnil conj []) (first more)))
          :else (recur more (update m :rest (fnil conj []) a)))))

(defn- report [{:keys [results html]} task]
  (println)
  (doseq [r results] (println (format "%-24s %s, %d prompts, %ds%s" (:arm r) (if (:ok? r) "done" "STOPPED") (:prompts r) (quot (:ms r) 1000)
                                      (if (:ok? r) "" (str "  (see " (or (:log r) (:error r)) ")")))))
  (println)
  (println "Report (one file, send it as it is):" html)
  (println (str "Live viewer: bb trace:gui   then open http://127.0.0.1:7360/runs/" task)))

(defn -main
  "bb runs:go ..."
  [& args]
  (let [o (opts args)
        task (slug (first (get o "task")))
        arms (mapv parse-arm (get o "arm"))]
    (when (or (str/blank? task) (empty? arms) (not (get o "prompts")))
      (println "usage: bb runs:go --task NAME --prompts DIR|FILE [--start DIR] [--check CMD] --arm [name=]MODEL[:EFFORT] ... [--serial]")
      (System/exit 2))
    (let [t (make-task! {:task task :start (first (get o "start")) :prompts (first (get o "prompts")) :check (first (get o "check"))})]
      (println (format "task %s: %d prompts, %d start files; arms: %s" task (:prompts t) (:files t) (str/join ", " (map :arm arms)))))
    (report (go! task arms {:serial? (boolean (get o "serial"))}) task)
    (System/exit 0)))

(defn html-main
  "bb runs:html TASK [FILE]"
  [& [task out]]
  (let [out (or out (var-dir "reports" (str task ".html")))]
    (fs/create-dirs (fs/parent out))
    (spit out (runs/static-html task))
    (println out)))

(defn- ask [q & [default]]
  (print (str q (when default (str " [" default "]")) " ")) (flush)
  (let [l (str/trim (or (read-line) ""))] (if (str/blank? l) default l)))

(defn new-main
  "bb runs:new: the questions, then the same as runs:go."
  [& _]
  (println "Compare models on the same prompts. Nothing is spent until you confirm.\n")
  (let [task (slug (ask "Name of this comparison:" "my-task"))
        start (ask "Folder to work in (a fresh copy of it is used per run):" ".")
        _ (println "Your prompts: a folder of .md/.txt files, or ONE file with the prompts separated by a line `---`.")
        prompts (ask "Where are they?")
        check (ask "Command that checks the result, e.g. `npm test` (empty: none):" "")
        _ (println "\nArms: `[name=]model[:effort]`, e.g. claude-haiku-4.5:low  or  gpt-5=gpt-5:high. Empty line to finish. (Use the model names /model shows in Copilot.)")
        arms (loop [acc []] (let [a (ask (str "Arm " (inc (count acc)) ":"))] (if (str/blank? a) acc (recur (conj acc a)))))
        arms (mapv parse-arm arms)
        ps (count (read-prompts prompts))]
    (println (format "\n%d arms x %d prompts = %d Copilot requests at least (each on its own model)." (count arms) ps (* (count arms) ps)))
    (if (= "y" (str/lower-case (ask "Run it? (y/n)" "n")))
      (do (make-task! {:task task :start start :prompts prompts :check (when-not (str/blank? check) check)})
          (report (go! task arms {}) task))
      (println "Nothing run."))))
