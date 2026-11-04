(ns trace.mirror-test
  "trace.mirror: oglinda stays off the recording path; the pure parts (verdict, text cut, a call as html);
  and, when oglinda is on this machine, the real pages of one mirror."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [trace.mirror :as m]))

(deftest oglinda-is-not-on-the-recording-path
  (testing "bb.edn does not depend on it"
    (is (not (str/includes? (slurp (System/getProperty "babashka.config")) "oglinda/"))))
  (testing "the hook, worker and recorder load without it"
    (let [r (p/shell {:out :string :err :string :continue true :dir (str (fs/parent (System/getProperty "babashka.config")))}
                     "bb" "-e" "(require 'trace.hook 'trace.worker 'trace.record) (println (boolean (some #(str/starts-with? (str %) \"oglinda\") (all-ns))))")]
      (is (zero? (:exit r)) (:err r))
      (is (= "false" (str/trim (:out r)))))))

(deftest verdict-says-cheaper-or-dearer
  (let [v @#'m/verdict]
    (is (= "cheaper" (:class (v {:cost 2.0 :busy 100} {:cost 1.0 :busy 50}))))
    (is (str/includes? (:text (v {:cost 2.0 :busy 100} {:cost 1.0 :busy 50})) "cheaper by 50%"))
    (is (= "dearer" (:class (v {:cost 1.0} {:cost 1.5}))))
    (is (= "same" (:class (v {:cost 1.0} {:cost 1.02}))))
    (is (= "none" (:class (v {:cost 1.0} nil))))))

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
      (is (str/includes? (:body (get* "/mirrors")) (subs id 0 8))))))
