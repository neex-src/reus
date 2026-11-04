(ns trace.copilot-install
  "Put reus into a user's Copilot: bb copilot:install / bb copilot:uninstall.

  Two files in the Copilot home ($COPILOT_HOME, else ~/.copilot), both found by the Copilot CLI and by the
  IntelliJ plugin (same engine, same home):
    hooks/reus.json              the hook file: every prompt, tool call and stop is handed to `bb -m trace.copilot`
                                 of THIS checkout (paths baked in; the command does nothing when the checkout is gone)
    skills/reus-compare/SKILL.md the guided flow (skills/reus-compare/SKILL.md of the checkout, with its path filled in);
                                 started in Copilot with /reus-compare
  Nothing else is touched, nothing is downloaded, no MCP server is added. Run it again after moving the checkout."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [trace.git :as g]))

(defn- q [s] (str "'" (str/replace (str s) "'" "'\\''") "'"))

(defn hook-command
  "The shell command of every hook: bb of this checkout, quiet and harmless when it cannot run."
  []
  (let [r g/home]
    (str "if [ -f " (q (fs/path r "bb.edn")) " ]; then mkdir -p " (q (fs/path r "var")) "; "
         (q g/bb) " --config " (q (fs/path r "bb.edn")) " -m trace.copilot 2>>" (q (fs/path r "var" "copilot-hook.err")) "; fi; exit 0")))

(defn hooks-json []
  (let [h [{"type" "command" "bash" (hook-command) "timeoutSec" 20}]]
    (json/generate-string
     {"version" 1
      "hooks" (into {} (map (fn [e] [e h]) ["UserPromptSubmit" "PostToolUse" "PostToolUseFailure" "Stop" "SessionEnd"]))}
     {:pretty true})))

(defn spit-hooks! [f]
  (fs/create-dirs (fs/parent f))
  (spit (str f) (str (hooks-json) "\n")))

(defn skill-text []
  (str/replace (slurp (str (fs/path g/home "skills" "reus-compare" "SKILL.md"))) "{{REUS}}" (str g/home)))

(defn home [] (or (System/getenv "COPILOT_HOME") (str (fs/path (fs/home) ".copilot"))))

(defn install!
  "Write the hook file and the skill into home (default: the Copilot home)."
  [& [home*]]
  (let [h (or home* (home))
        hooks (fs/path h "hooks" "reus.json")
        skill (fs/path h "skills" "reus-compare" "SKILL.md")]
    (spit-hooks! hooks)
    (fs/create-dirs (fs/parent skill))
    (spit (str skill) (skill-text))
    (println "installed in" h)
    (println "  " (str hooks) "(records every Copilot session into" (str g/*repo*) ")")
    (println "  " (str skill) "(start it in Copilot with /reus-compare)")
    (println "Viewer: bb trace:gui   (in" (str g/home) ")")))

(defn uninstall! [& [home*]]
  (let [h (or home* (home))]
    (fs/delete-if-exists (fs/path h "hooks" "reus.json"))
    (fs/delete-tree (fs/path h "skills" "reus-compare"))
    (println "removed reus from" h "(the traces are kept)")))
