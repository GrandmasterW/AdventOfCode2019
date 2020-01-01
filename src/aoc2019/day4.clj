(ns aoc2019.day4
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))




(defn contains-repeated-nums? [soi]
  "true if at least one number appears repeatedly, soi: set of integers representing chars"
  (> (count (filter true? (map = soi (rest soi)))) 0))

(defn never-less-digits? [soi]
  ""
  (= (dec (count soi))
     (count (filter true? (map <= soi (rest soi))))))

(defn double-digits? [soi]
  "true if soi contains at least one double digit like 122345"
  (let [p (partition-by identity soi)]
    (some #(= (count %1) 2) p))
  )

(defn pw-conform-p1? [x]
  (let [si (map int (str x))] ; convert to integers 
    (and (contains-repeated-nums? si)
         (never-less-digits? si))))

(defn pw-conform-p2? [x]
  (let [si (map int (str x))] ; convert to integers 
    (and (contains-repeated-nums? si)
         (never-less-digits? si)
         (double-digits? si))))

(defn gen-passwords [from to pred]
  "create conforming passwords between from and to"
  (filter pred (range from (inc to))))

(defn count-passwords-p1 [from to]
  (count (gen-passwords from to pw-conform-p1?)))
  
(defn count-passwords-p2 [from to]
  (count (gen-passwords from to pw-conform-p2?)))


;; specs
(s/def ::int6
  (s/and int?
         (partial > 1000000)
         (partial <   99999)
         pos?))

(s/fdef pw-conform-p1?
  :args (s/cat :x ::int6)
  :ret (s/or :true true :false false))

