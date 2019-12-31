(ns aoc2019.day3
  (:require [aoc2019.hvlines :refer :all])
  (:import [aoc2019.hvlines Point HVLine]))


(defn convert-direction [direction]
  "parse one direction like 'R8' into a map containing the direction key as :right, :left, :up, or :down and the distance"
  (let [tmp-direction (clojure.string/trim direction)
        dist-val (Integer/parseInt (subs tmp-direction 1))
        dir-key (case (first tmp-direction)
                  \R :right
                  \L :left
                  \U :up
                  \D :down
                  nil)]
      (hash-map dir-key dist-val)
    ))

(defn parse-wire [wire-str]
  "parse a string of directions like 'R8,D5,L3' into a sequence of sections, each containing :right, :left, :up, or :down and the distance"
  ;; turn into sequence of single strings
  ;; get a direction for each
  (let [seq-of-str (clojure.string/split wire-str #",")]
    (map convert-direction seq-of-str)))


(defn compute-new-pos [last-x last-y orientation distance]
  "compute a new position from last-x, last-y, orientation and distance of the movement"
  (case orientation
    :right {:x (+ last-x distance) :y last-y}
    :left  {:x (- last-x distance) :y last-y}
    :up    {:x last-x :y (+ last-y distance)}
    :down  {:x last-x :y (- last-y distance)}))


(defn next-point [result-vector direction]
  "computes the next point, given the sequence of results containing the last point and the next direction. Returns a vector of result-vector added by new element"
  (let [orientation (first (keys direction))
        distance (first (vals direction))
        last-pos (last result-vector)
        new-pos (compute-new-pos (:x last-pos) (:y last-pos) orientation distance)]
    (conj result-vector new-pos)))


(defn cons-wire [wire-str]
  "construct a data structure of sequential points for a wire-string, each point a map of :x and :y coordinates"
  (reduce next-point
          [{:x 0 :y 0}] ; starting vector containing origin
          (parse-wire wire-str))) ; convert string into a vector of directions

(defn cons-wire-parts [wire-string]
  "converts a wire-string into the points and returns a hvline for each part"
  (let [wire-points (cons-wire wire-string)]
    (mapv make-hvline wire-points (rest wire-points))))

(defn wire-intersections [wire1 wire2] 
  "Compute all points that are intersections of the wires except the origin 0,0"
  (remove
   #(or (nil? %1) (= (Point. 0 0) %1)) ; get rid of nil and 0,0
   (for [l1 wire1
         l2 wire2]
     (compute-intersections l1 l2)))) ; check all potential intersections -> a couple too many still, as a,b and b,a are treated... 

(defn shortest-distance [wire-str1 wire-str2]
  "Retrieve min value of all manhattan distances of all intersection points"
  (reduce min
          (mapv #(+ (Math/abs (:x %1))(Math/abs (:y %1))) ; manhattan distance computation
                (wire-intersections
                 (cons-wire-parts wire-str1)
                 (cons-wire-parts wire-str2)))))

(defn comp-steps [steps wire point]
  "Recursion helper: returns the number of steps to the given point as sum of steps on the lines on one wire (a vector of lines)"
  (let [line (first wire)
        point-val (point-at-line point line) 
        add-steps (if-not point-val (line-steps line) point-val)]
    (if (and point-val (rest wire))
      (+ steps add-steps)
      (recur (+ steps add-steps) (rest wire) point))))

(defn wire-min-steps [wire-str1 wire-str2]
  "find intersection with minimum total steps on both wires"
  (let [w1 (cons-wire-parts wire-str1)
        w2 (cons-wire-parts wire-str2)
        points (wire-intersections w1 w2)]
    (reduce min
            (mapv
             #(+ (comp-steps 0 w1 %1) (comp-steps 0 w2 %1)) ; add steps on both wires
             points))))


