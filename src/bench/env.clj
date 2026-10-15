(ns bench.env
  "Where the lame/sane bench keeps its data. reus is the dir of the bb.edn in use.
  A run is var/bench/<run> (BENCH_RUN, default run2) holding, per session s:
  <s>/logs (jsonl transcripts, turns.log), <s>/um-<s> (the session's checkout),
  plus results.edn, run.log and analysis/<s>-calls.edn. The prompts are
  work/bench/prompts.edn."
  (:require [babashka.fs :as fs]))

(def root (str (fs/parent (fs/canonicalize (System/getProperty "babashka.config")))))
(def sessions ["sane" "lame"])
(def prompts-file (str (fs/path root "work" "bench" "prompts.edn")))

(defn run-dir [] (str (fs/path root "var" "bench" (or (System/getenv "BENCH_RUN") "run2"))))
(defn path [& parts] (str (apply fs/path (run-dir) parts)))
(defn logs [s] (path s "logs"))
(defn checkout [s] (path s (str "um-" s)))
(defn transcript [s] (some-> (first (fs/glob (logs s) "*.jsonl")) str))
(defn calls-file [s] (path "analysis" (str s "-calls.edn")))
