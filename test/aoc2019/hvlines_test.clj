(ns aoc2019.hvlines-test
  (:require [clojure.test :refer :all]
            [aoc2019.hvlines :refer :all])
  (:import [aoc2019.hvlines Point HVLine]))

(deftest compute-orientation-test
  (testing "horizontal"
    (is (= :horizontal (compute-orientation (Point. 0 0) (Point. 8 0)))))
  (testing "vertical"
    (is (= :vertical (compute-orientation (Point. 0 0) (Point. 0 8)))))
  (testing "diagonal"
    (is (= :diagonal (compute-orientation (Point. 0 0) (Point. 8 8)))))
)

(deftest line-steps-test
  (testing "hline"
    (is (= 4 (line-steps (make-hvline (Point. 6 3)(Point. 2 3))))))
  (testing "vline"
    (is (= 4 (line-steps (make-hvline (Point. 6 7)(Point. 6 3))))))
  )


(deftest point-at-line-xy-test
  (testing ":x in line at horizontal"
    (is (= 1 (point-at-line-xy :x (Point. 4 5) (make-hvline (Point. 3 5)(Point. 8 5))))))
  (testing ":y in line at vertical"
    (is (= 3 (point-at-line-xy :y (Point. 4 5) (make-hvline (Point. 4 2)(Point. 4 6))))))
  (testing ":x not in line at horizontal"
    (is (nil? (point-at-line-xy :x (Point. 9 5) (make-hvline (Point. 3 5)(Point. 8 5))))))
  (testing ":y not in line at vertical"
    (is (nil? (point-at-line-xy :y (Point. 4 9) (make-hvline (Point. 4 2)(Point. 4 5))))))
  )

(deftest point-at-line-test
  (testing "Point at h line"
    (is (= 5 (point-at-line (Point. 5 0) (make-hvline (Point. 0 0) (Point. 8 0))))))
  (testing "Point at v line"
    (is (= 6 (point-at-line (Point. 0 6) (make-hvline (Point. 0 0) (Point. 0 8))))))
  (testing "Point not at h line"
    (is (nil? (point-at-line (Point. 0 5) (make-hvline (Point. 0 0) (Point. 8 0))))))
  )
  
(deftest compute-intersections-test
  (testing "h and v in one point"
    (is (= (Point. 6 5)
           (compute-intersections
            (make-hvline (Point. 8 5)(Point. 3 5))
            (make-hvline (Point. 6 7)(Point. 6 3))))))
  (testing "h and v in origin"
    (is (= (Point. 0 0)
           (compute-intersections
            (make-hvline (Point. 0 0)(Point. 6 0))
            (make-hvline (Point. 0 0)(Point. 0 3))))))
  (testing "h and v without match returns nil" 
    (is (nil?
         (compute-intersections
            (make-hvline (Point. 3 5)(Point. 8 5))
            (make-hvline (Point. 1 3)(Point. 1 7))))))
  (testing "h and h returns nil, even if overlapping" 
    (is (nil?
         (compute-intersections
            (make-hvline (Point. 8 5)(Point. 3 5))
            (make-hvline (Point. 4 5)(Point. 9 5))))))
  )
