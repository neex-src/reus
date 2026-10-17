(ns trace.ignore-test
  "trace.ignore against what git does: the same paths, `git check-ignore` in a throwaway repo."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [trace.ignore :as i]))

(def scratch (str (fs/path (fs/parent (System/getProperty "babashka.config")) ".scratch" "ignore-test")))

(def root-rules "node_modules/\n*.log\n/build\n!keep.log\n.scratch/\n# a comment\ndocs/**/tmp\nfoo/*.o\n**/gen/*.js\nx?z\n[ab]c.txt\n")
(def sub-rules "*.gen\n!/x.gen\n")

(def paths ["node_modules/a/b.js" "a/node_modules/x" "src/a.log" "keep.log" "build/x" "src/build/x" ".scratch/wt/a"
            "docs/a/b/tmp" "docs/tmp" "foo/a.o" "foo/b/a.o" "sub/a.gen" "sub/x.gen" "sub/d/x.gen" "a.txt" "src/gen/a.js"
            "gen/a.js" "xyz" "xz" "ac.txt" "cc.txt" "keep.log.txt"])

(deftest same-as-git
  (fs/delete-tree scratch)
  (fs/create-dirs (fs/path scratch "sub"))
  (spit (str (fs/path scratch ".gitignore")) root-rules)
  (spit (str (fs/path scratch "sub" ".gitignore")) sub-rules)
  (p/shell {:dir scratch :out :string :err :string} "git" "init" "-q")
  (let [git-says (set (str/split-lines
                       (:out (p/shell {:dir scratch :out :string :err :string :continue true :in (str (str/join "\n" paths) "\n")}
                                      "git" "check-ignore" "--stdin" "--no-index"))))
        rules {"" (i/parse root-rules) "sub" (i/parse sub-rules)}]
    (doseq [p paths]
      (is (= (contains? git-says p) (i/ignored? [] rules p)) p))))
