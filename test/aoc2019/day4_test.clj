(ns aoc2019.day4_test
  (:require [clojure.test :refer :all]
            [aoc2019.day4 :refer :all]))

(def from 152085)
(def to 670283)

(deftest pw-conform-p1?-test
  (testing "correct same chars"
    (is (pw-conform-p1? 111111)))
  (testing "correct same chars at beginning"
    (is (pw-conform-p1? 113456)))
  (testing "correct same chars twice"
    (is (pw-conform-p1? 122334)))
  )

(deftest count-passwords-p1-test
  (testing "controlled input"
    (is (= 2
           (count-passwords-p1 111110 111112))))
  (testing "aoc input"
    (is (= 1764
           (count-passwords-p1 from to)))))


(deftest pw-conform-p2?-test
  (testing "incorrect: all same characters"
    (is (not (pw-conform-p2? 111111))))
  (testing "double chars at beginning"
    (is (pw-conform-p2? 113456)))
  (testing "same chars twice"
    (is (pw-conform-p2? 122334)))
  (testing "incorrect: triple digit"
    (is (not (pw-conform-p2? 123444))))
  (testing "double digit and triple digit"
    (is (pw-conform-p2? 113444)))
  )


(deftest count-passwords-p2-test
  (testing "controlled input"
    (is (= 6
           (count-passwords-p2 113444 113450))))
  (testing "aoc input"
    (is (= 1196
           (count-passwords-p2 from to))))
  )
