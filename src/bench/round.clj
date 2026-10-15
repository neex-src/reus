(ns bench.round
  "Benchmark runner: send each prompt of work/bench/prompts.edn to both sessions (tmux
  window reus:reus-0001 = sane, reus:reus-0002 = lame), wait until both are idle, append
  wall seconds per session to <run>/results.edn.  bb bench:round [from] [to]"
  (:require [babashka.process :as p] [bench.env :as env] [clojure.edn :as edn] [clojure.string :as str]))

(def sessions {:sane {:logs (env/logs "sane") :win "reus:reus-0001" :dir (env/checkout "sane")}
               :lame {:logs (env/logs "lame") :win "reus:reus-0002" :dir (env/checkout "lame")}})
(def prompts (delay (edn/read-string (slurp env/prompts-file))))
(def results (env/path "results.edn"))
(defn tmux [& a] (:out (apply p/shell {:out :string :err :string :continue true} "tmux" a)))
(defn pane [w] (tmux "capture-pane" "-p" "-t" w))
(defn busy? [w] (str/includes? (pane w) "esc to interrupt"))
(defn send! [w text] (tmux "send-keys" "-t" w "-l" text) (Thread/sleep 400) (tmux "send-keys" "-t" w "Enter"))
(defn now [] (System/currentTimeMillis))
(defn merged? [k] ; the Stop hook of trace.bb has merged the prompt branch: worktree is back on its main branch
  (= (name k) (str/trim (:out (p/shell {:out :string :err :string :continue true :dir (:dir (sessions k))} "git" "branch" "--show-current")))))
(defn turns [k] (let [f (java.io.File. (str (:logs (sessions k)) "/turns.log"))]
                (if (.exists f) (count (str/split-lines (slurp f))) 0)))
(defn run-round [n]
  (let [before (into {} (for [k (keys sessions)] [k (turns k)]))
        t0 (now)]
    (doseq [[_ {:keys [win dir]}] sessions] (send! win (str/replace (nth @prompts n) "{dir}" dir)))
    (loop [done {}]
      (let [done (into done (for [k (keys sessions) :when (and (not (done k)) (> (turns k) (before k)) (merged? k))]
                              [k (quot (- (now) t0) 1000)]))]
        (cond (= (count done) (count sessions)) done
              (> (- (now) t0) (* 20 60 1000)) (merge {:sane :timeout :lame :timeout} done)
              :else (do (Thread/sleep 2000) (recur done)))))))

(defn -main [& args]
  (let [[from to] (map parse-long args)]
    (doseq [n (range (or from 0) (inc (or to (dec (count @prompts)))))]
      (let [r (run-round n)]
        (spit results (pr-str {:round n :prompt (nth @prompts n) :secs r}) :append true)
        (spit results "\n" :append true)
        (println n r)))
    (spit (env/path ".done") "done")))
