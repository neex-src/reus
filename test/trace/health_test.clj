(ns trace.health-test
  "trace.health's unrecorded sessions against fake transcripts under <reus>/.scratch/health-test: a session of
  reus' own e2e runs is not a real one; a session working in a .scratch worktree is."
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [trace.health :as health]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "health-test")))

(deftest test-scratch
  (doseq [c ["/h/reus/.scratch/hidden/.scratch/e2e/20261104-035839/proj" "/h/reus/.scratch/e2e-20261104-072424/proj"
             "/h/reus/.scratch/e2e/x" "/h/reus/.scratch/e2eproj"]]
    (is (health/test-scratch? c) c))
  (doseq [c [nil "" "/h/reus" "/h/reus/.scratch/wt-moot-out" "/h/reus/.scratch/wt-e2e-fix/src" "/h/p/e2e/proj"]]
    (is (not (health/test-scratch? c)) (str c))))

(deftest unrecorded-skips-e2e-sessions
  (fs/delete-tree scratch)
  (let [claude (str (fs/path scratch "claude")) marks (str (fs/path scratch "seen"))
        t (fn [proj session cwd]
            (let [f (fs/path claude "projects" proj (str session ".jsonl"))]
              (fs/create-dirs (fs/parent f))
              (spit (str f) (str "{\"type\":\"permission-mode\"}\n"
                                 (when cwd (str "{\"type\":\"user\",\"cwd\":\"" cwd "\",\"timestamp\":\"" (java.time.Instant/now) "\"}\n"))))
              (fs/set-last-modified-time f (fs/millis->file-time (- (System/currentTimeMillis) 300000)))))]
    (fs/create-dirs claude)
    (spit (str (fs/path claude "settings.json")) "{\"hooks\":{\"x\":\"trace.hook\"}}")
    (fs/create-dirs marks)
    (spit (str (fs/path marks ".since")) "")
    (fs/set-last-modified-time (fs/path marks ".since") (fs/millis->file-time (- (System/currentTimeMillis) 7200000)))
    (t "-a" "e2e1" "/h/reus/.scratch/hidden/.scratch/e2e/20261104-035839/proj")
    (t "-b" "real1" "/h/reus/.scratch/wt-x")
    (t "-c" "nocwd" nil)
    (binding [health/*claude-dir* claude health/*seen-dir* marks]
      (is (= #{"real1" "nocwd"} (set (map :session (health/unrecorded))))))))
