(ns user
  (:require
   [clojure.string :as str]
   [clj-http.client :as client]
   [cheshire.core :as chesire]
   )
  )

;; stolen from https://gist.github.com/hozumi/1472865
(defn sha1-str [s]
  (->> (-> "sha1"
           java.security.MessageDigest/getInstance
           (.digest (.getBytes s)))
       (map #(.substring
              (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))

(defn enc-pass [pass] (sha1-str (str pass "JarJarBinks9")))

(def my-cookie-store (clj-http.cookies/cookie-store))

(def wordfeud-api-url "http://api.wordfeud.com/wf/")

(defn post-wordfeud [sub-url]
  (client/post (str wordfeud-api-url sub-url)
               {:cookie-store my-cookie-store}))

(defn log-in [credentiols]
  (client/post (str wordfeud-api-url "user/login/email/")
               {:content-type :json
                :form-params credentiols
                :cookie-store my-cookie-store}))

(defn json->clojure [json] (chesire/parse-string json (comp keyword #(str/replace % \_ \-))))

(defn get-wordfeud [sub-url]
  (let [response (post-wordfeud sub-url)]
    (if (not= (response :status) 200)
      (throw (ex-info "Response not status not 200" {:status (response :status) :response response})))
    (let [edn-body (json->clojure (response :body))]
      (if (not= "success" (edn-body :status))
        (throw (ex-info "Response not success" {:status (edn-body :status) :body edn-body})))
      (edn-body :content))))

(defn get-games [] ((get-wordfeud "user/games/") :games))

(defn get-game [game-id] ((get-wordfeud (str "game/" game-id \/)) :game))

(defn wordfeud-state->internal-state [{:keys [tiles players]}]
  (let [local-player (first (filter :is-local players))]
    {:tiles (into {} (map (fn [[x y s]] [{:x x :y y} (first s)]) tiles))
     :rack (str/join (local-player :rack))}))

(defn get-4-neighbours [{:keys [x y]}]
  (let [one-dim-neighbours
        (fn [n] (concat (if (> n 0) [(dec n)])
                        (if (< n 14) [(inc n)])))]
    (concat (for [nx (one-dim-neighbours x)] {:x nx :y y})
            (for [ny (one-dim-neighbours y)] {:x x :y ny}))))

(defn get-game-tile-neighbours [{:keys [tiles]}]
  (let [tiles (set (keys tiles))
        tile-neighbours (reduce (fn [acc tile] (into acc (get-4-neighbours tile))) #{} tiles)]
    (clojure.set/difference tile-neighbours tiles)))

(defn print-state [map]
  (doseq [y (range 0 15)]
    (println (apply str (for [x (range 0 15)] (map {:x x :y y} \ ))))))

(comment
  (def email "")
  (def password (enc-pass ""))
  (def credentials
    {:email email
     :password password})
  (log-in credentials)
  (get-games)
  )
