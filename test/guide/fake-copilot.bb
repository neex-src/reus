#!/usr/bin/env bb
;; a stand-in for the copilot CLI in trace.runner-test: logs its words, makes ok.txt
(spit (System/getenv "FAKE_LOG") (str (pr-str *command-line-args*) "\n") :append true)
(spit "ok.txt" "ok\n")
