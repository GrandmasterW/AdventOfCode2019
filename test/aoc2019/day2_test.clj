(ns aoc2019.day2-test
  (:require [clojure.test :refer :all]
            [aoc2019.day2 :refer :all]))

;; Day 2 - Part 1


(deftest intcode-test
  (testing "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)."
    (is (= (intstart [1,0,0,0,99]) [2,0,0,0,99])))
  (testing "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)."
    (is (= (intstart [2,3,0,3,99]) [2,3,0,6,99])))
  (testing "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)."
    (is (= (intstart [2,4,4,5,99,0]) [2,4,4,5,99,9801])))
  (testing "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99."
    (is (= (intstart [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))))

;; Day 2 - part 2
(deftest intcode-noun-verb-test
  (testing "set-noun-verb: 1,0,0,0,99,0,0 with noun 5 and verb 6 becomes 1,5,6,0,99,0,0"
    (is (= (set-noun-verb [1,0,0,0,99,0,0] 5 6) [1,5,6,0,99,0,0])))
  (testing "set-vars-and-start: 1,0,0,0,99,1,2 with noun 5 and verb 6 results in 3,5,6 "
    (is (= (set-vars-and-start [1,0,0,0,99,1,2] 5 6) [3 5 6])))
  (testing "set-vars-and-start: 2,0,0,0,99,2,2 with noun 5 and verb 6 results in 4,5,6 "
    (is (= (set-vars-and-start [2,0,0,0,99,2,2] 5 6) [4 5 6])))
  (testing "set-vars-and-start: 1,0,0,0,99,6,7 with result 13 has noun/verb: 5,6 / 6,5"
    (is (= (set-vars-and-start [1,0,0,0,99,6,7] 5 6) [13 5 6])))
  (testing "find-day2-vars: 1,0,0,0,99,6,7 with result 13 has at least noun/verb: 5,6 / 6,5"
    (is (contains?
         (set (find-day2-vars [1,0,0,0,99,6,7] 13)) [13 5 6])))
  )




                                           
