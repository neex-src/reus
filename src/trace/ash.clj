(ns trace.ash
  "ashamer, the library of the general part of the fn layer (~/projects/ashamer), found at run time.

  It is NOT in bb.edn :deps: bb resolves those for every task, the hook's too, and the recording path must
  not need ashamer (the sandbox has none). Only the fn layer asks for it, through f: ASHAMER_HOME if that is set
  (then only there), else ~/projects/ashamer. Absent, `bb trace:fns` says so; nothing else notices."
  (:require [babashka.classpath :as cp]
            [babashka.fs :as fs]))

(defn home [] (or (System/getenv "ASHAMER_HOME") (str (fs/path (System/getProperty "user.home") "projects" "ashamer"))))

(defn available? [] (fs/exists? (fs/path (home) "src" "ashamer" "fns.clj")))

(defonce ^:private loaded (atom false))

(defn load!
  "Put ashamer on the classpath (once); throws ex-info {:ashamer :missing} when it is not there."
  []
  (when-not @loaded
    (when-not (available?)
      (throw (ex-info (str "ashamer is not installed (looked in " (home) "; ASHAMER_HOME says where): the fn layer needs it")
                      {:ashamer :missing})))
    (cp/add-classpath (str (fs/path (home) "src")))
    (require 'ashamer.fns 'ashamer.git)
    (reset! loaded true)))

(defn f
  "The var of ashamer's fully qualified symbol sym, loading ashamer first."
  [sym]
  (load!)
  (requiring-resolve sym))

(defn missing? [e] (= :missing (:ashamer (ex-data e))))

(defn in-repo
  "Call thunk with ashamer.git's repo bound to repo."
  [repo thunk]
  (with-bindings {(f 'ashamer.git/*repo*) (str repo)} (thunk)))
