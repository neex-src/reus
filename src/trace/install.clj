(ns trace.install
  "Switch the global trace hook (trace.hook) on or off in ~/.claude/settings.json: the same on every
  machine, nothing else to set (the trace repo and the projects root follow from where this checkout
  is, trace.git/home). Other hooks are kept; ours are the ones whose command names trace.hook (or
  trace.record, the older entry point), and `on` again replaces them."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [trace.git :as g]
            [trace.disk :as disk]
            [trace.record :as rec]))

(def settings (str (fs/path (fs/home) ".claude" "settings.json")))

(defn- ours? [entry] (some #(re-find #"trace\.(record|hook)\b" (str (get % "command"))) (get entry "hooks")))

(defn- hook [cmd] [{"hooks" [{"type" "command" "command" cmd "timeout" 15}]}])

(defn hook-entries
  "The hooks settings for the bb.edn that has trace.hook."
  [bb-edn]
  (let [cmd #(str "bb --config " bb-edn " -m trace.hook " %)]
    {"UserPromptSubmit" (hook (cmd "prompt"))
     "PostToolUse" (hook (cmd "tool"))
     "PostToolUseFailure" (hook (cmd "tool"))
     "SubagentStart" (hook (cmd "subagent-start"))
     "SubagentStop" (hook (cmd "subagent-stop"))
     "Stop" (hook (cmd "stop"))
     "SessionEnd" (hook (cmd "stop"))}))

(defn- rewrite! [f]
  (let [s (json/parse-string (slurp settings))
        hooks (into {} (keep (fn [[ev es]] (let [es (vec (remove ours? es))] (when (seq es) [ev es])))
                             (get s "hooks")))]
    (fs/copy settings (str settings ".bak") {:replace-existing true})
    (spit settings (json/generate-string (assoc s "hooks" (f hooks)) {:pretty true}))))

(defn on
  "Install the hook; bb-edn is the absolute path of the bb.edn that has trace.hook."
  [bb-edn]
  (rec/ensure-repo!)
  (rewrite! #(merge-with into % (hook-entries bb-edn)))
  (println "trace on: every Claude session in a git repo (or" (str (disk/projects-root) "/<x>)") "records into" g/*repo*)
  (println "settings:" settings "(previous version in settings.json.bak)"))

(defn off []
  (rewrite! identity)
  (println "trace off; the trace repo" g/*repo* "is kept"))
