(ns trace.mirror-test
  "trace.mirror: oglinda stays off the recording path; the pure parts (verdict, text cut, a call as html);
  and, when oglinda is on this machine, the real pages of one mirror."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.mirror :as m]
            [trace.page :as pg]))

(deftest oglinda-is-not-on-the-recording-path
  (testing "bb.edn does not depend on it"
    (is (not (str/includes? (slurp (System/getProperty "babashka.config")) "oglinda/"))))
  (testing "the hook, worker and recorder load without it"
    (let [r (p/shell {:out :string :err :string :continue true :dir (str (fs/parent (System/getProperty "babashka.config")))}
                     "bb" "-e" "(require 'trace.hook 'trace.worker 'trace.record) (println (boolean (some #(str/starts-with? (str %) \"oglinda\") (all-ns))))")]
      (is (zero? (:exit r)) (:err r))
      (is (= "false" (str/trim (:out r)))))))

(deftest verdict-is-computed-and-neutral
  (let [v m/verdict]
    (testing "what the numbers say, in words, whichever side is better"
      (is (= "less" (:class (v {:cost 2.0 :busy 100} {:cost 1.0 :busy 50}))))
      (is (= "The twin cost 50% less ($1.00 against $2.00) and was busy for 50% less time (50s against 1m 40s)."
             (:text (v {:cost 2.0 :busy 100} {:cost 1.0 :busy 50}))))
      (is (= "more" (:class (v {:cost 1.0 :busy 10} {:cost 1.5 :busy 10}))))
      (is (= "The twin cost 4% more ($1.04 against $1.00) and was busy for 38% less time (1m 02s against 1m 40s)."
             (:text (v {:cost 1.0 :busy 100.0} {:cost 1.04 :busy 62.0}))))
      (is (= "same" (:class (v {:cost 1.0 :busy 10} {:cost 1.001 :busy 10}))))
      (is (str/includes? (:text (v {:cost 1.0 :busy 10} {:cost 1.001 :busy 10})) "cost about the same")))
    (testing "no value word: a cheaper twin is not good news, a dearer one is not bad news"
      (doseq [x [(v {:cost 2.0 :busy 100} {:cost 1.0 :busy 50}) (v {:cost 1.0 :busy 50} {:cost 2.0 :busy 100})]]
        (is (not (re-find #"(?i)\b(better|worse|good|bad|saves|saved|wins|cheaper|dearer|faster|slower)\b" (:text x))))))
    (testing "a side that failed is said first"
      (is (str/starts-with? (:text (v {:cost 1.0 :busy 10 :interrupted true} {:cost 1.0 :busy 10}))
                            "The real session's turn was interrupted by the user. The twin cost"))
      (is (str/starts-with? (:text (v {:cost 1.0 :busy 10} {:cost 1.0 :busy 10 :error :limit}))
                            "The twin's turn ended with an error (limit). The twin cost"))
      (is (str/starts-with? (:text (v {:cost 1.0 :busy 10} {:cost 1.0 :busy 10 :error "success"})) "The twin cost"))
      (is (= "The twin has not run this prompt." (:text (v {:cost 1.0} nil))))
      (is (= "none" (:class (v {:cost 1.0} nil)))))))

(deftest a-session-sentence-says-what-went-wrong-first
  (let [sm {:both 3 :prompts 5 :not-run 2 :real-interrupted 1 :twin-interrupted 0 :twin-errors 1 :state :failed :reason "died"
            :real-cost 10.0 :twin-cost 11.0 :both-real-busy 100.0 :both-twin-busy 50.0 :nod-calls 54 :nod-failed 5}]
    (is (= (str "The twin's run failed (died). The twin has not run 2 of 5 prompts. "
                "The real session's turn was interrupted by the user on 1 prompt. The twin's turn ended with an error on 1 prompt. "
                "Over the 3 prompts both sides ran: the twin cost 10% more ($11.00 against $10.00) and was busy for 50% less time (50s against 1m 40s). "
                "nod was called 54 times; 5 failed.")
           (m/session-sentence sm)))
    (is (= "Over the prompt both sides ran: the twin cost 2% more ($10.20 against $10.00) and was busy for about the same time (1m 40s against 1m 40s). nod was called 0 times."
           (m/session-sentence {:both 1 :prompts 1 :not-run 0 :real-interrupted 0 :twin-interrupted 0 :twin-errors 0 :real-cost 10.0 :twin-cost 10.2
                                :both-real-busy 100.0 :both-twin-busy 100.0 :nod-calls 0 :nod-failed 0})))))

(deftest oglinda-is-found-where-the-machine-keeps-it
  (testing "OGLINDA_HOME is the only place when set"
    (is (= ["/x/og"] (m/candidates "/x/og" "/h/prog/reus" "/h"))))
  (testing "else beside this checkout, in the sandbox layout, in the projects layout; nothing is a the host path by itself"
    (is (= ["/h/prog/oglinda" "/h/projects/oglinda"] (m/candidates nil "/h/prog/reus" "/h")))
    (is (= ["/w/oglinda" "/h/prog/oglinda" "/h/projects/oglinda"] (m/candidates "" "/w/reus" "/h")))
    (is (not-any? #(str/includes? % "user") (m/candidates nil "~/projects/reus" "/home/claude")))))

(deftest the-pages-are-plain
  (testing "the stylesheet has nothing that scrolls, clips or is shorter than its text"
    (is (not (re-find #"max-height|overflow:\s*(auto|scroll|hidden|clip)|overflow-[xy]:\s*(auto|scroll|hidden|clip)|text-overflow" pg/css))))
  (testing "a page is one self-contained document"
    (let [html (pg/shell "t" [:p "x"] :current :mirrors)]
      (is (not (re-find #"<script|<link|https?://|@import|@font-face" html))))))

(deftest long-text-is-cut-not-scrolled
  (let [html (str (hiccup2.core/html (@#'m/text-block (str/join "\n" (repeat 500 "a line of output")) 1000 "out")))]
    (is (str/includes? html "<details"))
    (is (str/includes? html "the other"))))

(deftest a-failed-call-shows-its-error-first
  (let [html (str (hiccup2.core/html
                   (@#'m/call-view {:i 3 :name "mcp__nod__eval" :nod? true :failed true :error "error: no such thing" :label "(show x)"
                                    :input {:code "(show x)" :cwd "/x"} :text "error: no such thing\nstopped" :dur 0.4})))]
    (is (str/includes? html "call failed nod"))
    (is (str/includes? html "FAILED"))
    (is (< (str/index-of html "error: no such thing") (str/index-of html "code sent to nod")))))

(deftest real-pages-render
  (when (m/available?)
    (let [id (first (m/mirror-ids))
          get* (fn [uri] (m/route uri {}))]
      (is (= 200 (:status (get* (str "/mirror/" (subs id 0 8))))))
      (is (= 200 (:status (get* (str "/mirror/" (subs id 0 8) "/p/1")))))
      (is (str/includes? (:body (get* "/mirrors")) (subs id 0 8)))
      (doseq [uri ["/mirrors" (str "/mirror/" (subs id 0 8)) (str "/mirror/" (subs id 0 8) "/p/1")]
              :let [html (:body (get* uri))]]
        (is (not (re-find #"<script|<link|(src|href)=\"https?:" html)) uri)))))
