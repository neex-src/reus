(ns bench.parity2
  "Parity with data: seed 12 users (@seed.io) into both apps, compare paging, sort and
  search results, clean up."
  (:require [babashka.http-client :as http] [clojure.string :as str]))

(def apps {:sane "http://127.0.0.1:8081" :lame "http://127.0.0.1:8082"})
(defn req [b method path & [form]]
  (str (:body (http/request {:method method :uri (str b path) :throw false
                             :headers (when form {"Content-Type" "application/x-www-form-urlencoded"})
                             :body (when form (str/join "&" (for [[k v] form] (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8")))))}))))
(def names [["Zoe" "Adams"] ["Yan" "Baker"] ["Xia" "Clark"] ["Walt" "Dunn"] ["Vera" "Evans"] ["Uma" "Frost"]
            ["Tom" "Gray"] ["Sam" "Hill"] ["Rae" "Ives"] ["Quin" "Jones"] ["Pia" "Kent"] ["Ola" "Lane"]])
(defn emails [body] (vec (filter #(str/ends-with? % "seed.io") (map second (re-seq #"([\w.+-]+@[\w.-]+\.\w+)" body)))))

(defn -main [& args]
  (doseq [[k b] apps]
    (doseq [[i [f l]] (map-indexed vector names)]
      (req b :post "/user/save" {:first_name f :last_name l :email (str (str/lower-case f) i "@seed.io") :department_id (inc (mod i 3))})))
  (doseq [path ["/user/list?q=seed" "/user/list?q=seed&page=2" "/user/list?q=seed&page=3" "/user/list?q=seed&page=4"
                "/user/list?q=seed&sort=email" "/user/list?q=seed&sort=last_name" "/user/list?q=seed&sort=email&page=2"
                "/user/list?q=ADAMS" "/user/list?q=seed.io%20" "/user/export.csv?q=seed&sort=email"]]
    (let [r (into {} (for [[k b] apps] [k (let [body (req b :get path)]
                                            (if (str/includes? path "csv") (count (str/split-lines body)) (emails body)))]))]
      (println path (if (= (:sane r) (:lame r)) (str "SAME " (pr-str (:sane r))) (str "
   DIFF sane=" (pr-str (:sane r)) "
        lame=" (pr-str (:lame r)))))))
  (doseq [[_ b] apps]
    (doseq [id (distinct (map second (re-seq #"/user/delete/(\d+)" (req b :get "/user/list?q=probe.io"))))] (req b :post (str "/user/delete/" id)))
    (doseq [p [1 2 3 4]] (doseq [id (distinct (map second (re-seq #"/user/delete/(\d+)" (req b :get (str "/user/list?q=seed.io&page=" p)))))] (req b :post (str "/user/delete/" id))))))
