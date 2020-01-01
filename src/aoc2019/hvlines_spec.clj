(ns aoc2019.hvlines-spec
  (:require [aoc2019.hvlines :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

;; specs
(s/def ::point-record
  (s/and
   record?
   (s/keys :req-un [::x ::y])))

(s/def ::hvline-record
  (s/and
   record?
   (s/keys :req-un [::orientation ::from ::to])))


(s/fdef point
  :args (s/cat :x int? :y int?)
  :ret ::point-record)

