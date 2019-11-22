(ns user
  (:require
   [clojure.string :as str]
   )
  )

(defn parse-line [line]
  (let [[word kind] (str/split line #";")]
    {:word word :kind kind}))

(defn parse-word [line]
  (let [ro-line-regex #"([1-9]. )?([^;]+);.*"]
    (last (re-matches ro-line-regex line))))

(defn parse-kind [line]
  (let [ro-line-regex #"[^;]+;(.*)"]
    (last (re-matches ro-line-regex line))))

(defn parse-word-kind [line]
  (let [ro-line-regex #"([1-9]. )?([^;]+);(.*)"
        [word kind] (re-matches ro-line-regex line)]
    {:word word :kind kind}))

(def danish-words (str/split-lines (slurp "RO2012.opslagsord.med.homnr.og.ordklasse.txt")))

(def word-set (into #{} (map (comp str/lower-case parse-word) danish-words)))

(defn exists [word] (some #(= word %) word-set))

(def not-in-ro #{"ionpar"})

(def conjugations #{"skreds"})

(def word-kinds (reduce (fn [acc l]
                          (let [word (parse-word l)
                                kind (parse-kind l)
                                kinds (acc word #{})
                                ] (assoc acc word (conj kinds kind)))) {} danish-words))

(defn coll->histogram [coll]
  (reduce (fn [acc x] (update acc x #(if % (inc %) 1))) {} coll))

(comment
  (exists "skreds")
  )
