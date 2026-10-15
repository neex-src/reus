#!/usr/bin/env bb
;; Claude Code hook (Stop, SessionEnd): copy the session transcript into
;; <dir given as arg 1, else cwd/logs>/<session_id>.jsonl so logs live with the session's own
;; directory and nobody has to dig through ~/.claude. Idempotent: overwrites.
;; On Stop it also appends "<epoch-ms> <session_id>" to logs/turns.log, the
;; turn-end marker the benchmark runner (bench.round) waits on.
(require '[babashka.fs :as fs] '[cheshire.core :as json])
(let [{:strs [hook_event_name transcript_path session_id cwd]} (json/parse-string (slurp *in*))]
  (when (and transcript_path cwd (fs/exists? transcript_path))
    (let [dir (fs/path (or (first *command-line-args*) (fs/path cwd "logs")))]
      (fs/create-dirs dir)
      (fs/copy transcript_path (fs/path dir (str session_id ".jsonl")) {:replace-existing true})
      (when (= hook_event_name "Stop")
        (spit (str (fs/path dir "turns.log")) (str (System/currentTimeMillis) " " session_id "\n") :append true)))))
