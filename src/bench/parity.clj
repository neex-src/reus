(ns bench.parity
  "Black-box parity check: the same HTTP probes against both apps (sane :8081, lame :8082),
  one row per probe. Creates and removes test users."
  (:require [babashka.http-client :as http] [cheshire.core :as j] [clojure.string :as str]))

(def apps {:sane {:base "http://127.0.0.1:8081" :dept "/departments"}
           :lame {:base "http://127.0.0.1:8082" :dept "/department/list"}})
(defn req [app method path & [{:keys [form]}]]
  (try (let [r (http/request {:method method :uri (str (:base app) path) :throw false :follow-redirects false
                              :headers (when form {"Content-Type" "application/x-www-form-urlencoded"})
                              :body (when form (str/join "&" (for [[k v] form] (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8")))))})]
         (update r :body str))
       (catch Exception e {:status :ERR :body (.getMessage e)})))
(defn rows [r] (count (re-seq #"(?i)<tr[^>]*>(?:(?!</tr>).)*(?:@|class=\"email\")" (str/replace (:body r) "\n" " "))))
(defn emails [r] (vec (map second (re-seq #"([\w.+-]+@[\w.-]+\.\w+)" (:body r)))))
(def probes
  [["home has new welcome"       (fn [a] (boolean (re-find #"Team Directory" (:body (req a :get "/")))))]
   ["list status/rows"           (fn [a] (let [r (req a :get "/user/list")] [(:status r) (rows r)]))]
   ["q=hopper"                   (fn [a] (emails (req a :get "/user/list?q=hopper")))]
   ["q=zzz (none)"               (fn [a] (rows (req a :get "/user/list?q=zzz")))]
   ["q=% (LIKE wildcard)"        (fn [a] (rows (req a :get "/user/list?q=%25")))]
   ["q=' (quote)"                (fn [a] (:status (req a :get "/user/list?q=%27")))]
   ["sort=email order"           (fn [a] (take 3 (emails (req a :get "/user/list?sort=email"))))]
   ["sort=last_name order"       (fn [a] (take 3 (emails (req a :get "/user/list?sort=last_name"))))]
   ["sort=bogus"                 (fn [a] (:status (req a :get "/user/list?sort=bogus")))]
   ["sort injection"             (fn [a] (:status (req a :get "/user/list?sort=email;drop%20table%20addressbook")))]
   ["page=1 rows"                (fn [a] (rows (req a :get "/user/list?page=1")))]
   ["page=2 rows"                (fn [a] (rows (req a :get "/user/list?page=2")))]
   ["page=999 / abc / -1"        (fn [a] (mapv #(:status (req a :get (str "/user/list?page=" %))) ["999" "abc" "-1"]))]
   ["csv status/type"            (fn [a] (let [r (req a :get "/user/export.csv")] [(:status r) (get-in r [:headers "content-type"])]))]
   ["csv header+lines"           (fn [a] (let [ls (str/split-lines (:body (req a :get "/user/export.csv")))] [(first ls) (count ls)]))]
   ["csv honors q"               (fn [a] (count (str/split-lines (:body (req a :get "/user/export.csv?q=hopper")))))]
   ["health"                     (fn [a] (let [r (req a :get "/health")] [(:status r) (get-in r [:headers "content-type"]) (try (j/parse-string (:body r)) (catch Exception _ :not-json))]))]
   ["save: empty first name"     (fn [a] (let [r (req a :post "/user/save" {:form {:first_name "" :last_name "Zed" :email "z@example.net" :department_id 1}})]
                                     [(:status r) (boolean (re-find #"(?i)required|must|empty|blank|cannot|can't" (:body r))) (boolean (re-find #"Zed" (:body r)))]))]
   ["save: email w/o @"          (fn [a] (let [r (req a :post "/user/save" {:form {:first_name "A" :last_name "B" :email "nope" :department_id 1}})]
                                     [(:status r) (boolean (re-find #"(?i)@|email" (:body r)))]))]
   ["save: ok (creates Tess)"    (fn [a] (let [r (req a :post "/user/save" {:form {:first_name "Tess" :last_name "O\"Brien, Jr" :email "tess@example.com" :phone "555-1" :department_id 1}})]
                                     [(:status r) (boolean (re-find #"tess@example.com" (:body (req a :get "/user/list?q=tess"))))]))]
   ["csv quoting of Tess"        (fn [a] (first (filter #(str/includes? % "tess@") (str/split-lines (:body (req a :get "/user/export.csv?q=tess"))))))]
   ["save: duplicate email"      (fn [a] (let [r (req a :post "/user/save" {:form {:first_name "Dup" :last_name "Tess" :email "TESS@example.com" :department_id 1}})]
                                     [(:status r) (boolean (re-find #"(?i)already|duplicate|exists|taken|unique|in use" (:body r)))]))]
   ["xss escaped in list"        (fn [a] (req a :post "/user/save" {:form {:first_name "<script>x</script>" :last_name "Xss" :email "xss@example.com" :department_id 1}})
                                   (let [b (:body (req a :get "/user/list?q=xss"))] [(boolean (re-find #"<script>x" b)) (boolean (re-find #"&lt;script" b))]))]
   ["phone shown"                (fn [a] (boolean (re-find #"555-1" (:body (req a :get "/user/list?q=tess")))))]
   ["GET delete rejected"        (fn [a] (:status (req a :get "/user/delete/1")))]
   ["form/9999"                  (fn [a] (:status (req a :get "/user/form/9999")))]
   ["dept page"                  (fn [a] (let [r (req a :get (:dept a))] [(:status r) (boolean (re-find #"Engineering" (:body r)))]))]
   ["dept add"                   (fn [a] (let [r (req a :post (str/replace (:dept a) "list" "save") {:form {:name "Probes"}})]
                                     [(:status r) (boolean (re-find #"Probes" (:body (req a :get (:dept a)))))]))]
   ["dept add duplicate"         (fn [a] (let [r (req a :post (str/replace (:dept a) "list" "save") {:form {:name "Probes"}})] (:status r)))]
   ["dept add empty"             (fn [a] (:status (req a :post (str/replace (:dept a) "list" "save") {:form {:name ""}})))]])

(defn -main [& args]
  (doseq [[label f] probes]
    (let [res (into {} (for [[k a] apps] [k (try (f a) (catch Exception e (str "ERR " (.getMessage e))))]))]
      (println (format "%-26s %s" label (if (= (:sane res) (:lame res)) (str "SAME  " (pr-str (:sane res)))
                                            (str "DIFF  sane=" (pr-str (:sane res)) "  lame=" (pr-str (:lame res))))))))
  ;; cleanup: delete probe users via POST (ids from the list page links)
  (doseq [[_ a] apps]
    (doseq [id (distinct (map second (re-seq #"/user/delete/(\d+)" (:body (req a :get "/user/list?q=probe.io")))))]
      (req a :post (str "/user/delete/" id)))))
