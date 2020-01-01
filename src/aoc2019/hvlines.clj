(ns aoc2019.hvlines)
  
(defrecord Point [x y]) ; set x and y as usual
(defrecord HVLine [orientation from to]) ; orientation should by :vertical or :horizontal, contains two Point-s

(defn point [x y]
  "typing helper to create a Point - less typing..."
  (Point. x y))

(defn compute-orientation [p1 p2]
  "returns the orientation for the resulting line p1 to p2"
  (cond
    (= (:x p1) (:x p2)) :vertical
    (= (:y p1) (:y p2)) :horizontal
    :else :diagonal))

(defn make-hvline [from to]
  "Creates a record of hvline from the Points from and to and derives the orientation :vertical, :horizontal or :diagonal from the values"
  (if-not (= from to)
    (HVLine. (compute-orientation from to) from to)))

(defn hvline [from to]
  "typing helper to create a HVLine"
  (make-hvline from to))


;;
;; methods
;;


(defn line-steps [line]
  "Returns the total number of steps / points of that line"
  (let [points (vals (rest line)) ; only the points
        [start end] (case (:orientation line)
                      :horizontal (mapv :x points)
                      :vertical (mapv :y points)
                      nil)]
    (if (and start end)
      (Math/abs (- end start)))))

(defn interval-contains-val [val i1 i2]
  "If val is between i1 and i2 or i2 and i1 (integers only), than return val"
  (let [lower (min i1 i2)
        upper (max i1 i2)]
    (if (and (>= val lower)(<= val upper))
      val)))

(defn point-at-line-xy [key point line]
  "check if the x or y value of point is in the interval of x or y of the line. If so, returns the difference of the point-value and the minimum line-value."
    (if-let [val (interval-contains-val (key point) (key (:from line)) (key (:to line)))]
      (Math/abs (- val (key (:from line)))))
  )


(defn point-at-line [point line]
  "If the point is on the line, return the line points / segments / steps from 'from' till the point. Nil else. Diagonal not implemented yet"
  (let [x (point-at-line-xy :x point line)
        y (point-at-line-xy :y point line)]
    (if (and x y)
      (case (:orientation line)
        :horizontal x 
        :vertical y
        nil))))

(defn line-contains [key l1 l2]
  "extracts value for key (:x, :y) to compute if val is in range of l2"
  (interval-contains-val (key (:from l1)) (key (:from l2)) (key (:to l2))))

(defn comp-one-point [line1 line2]
  "Returns the one if any matching point for a horizontal and a vertical line - others not implemented!"
  (let [y (if (= (:orientation line1) :horizontal)
            (line-contains :y line1 line2)  ; check if y1 is between the y-range of line2
            (line-contains :y line2 line1)) ; o2 must be horizontal, the other way round 
        x (if (= (:orientation line1) :vertical)
            (line-contains :x line1 line2)  ; check if x1 is between x-range of line 2
            (line-contains :x line2 line1))] ; else o2 must be vertical, same logic
    (if (and x y) (Point. x y))))

(defn compute-intersections [line1 line2]
  "Returns a vector of points in which the hvlines intersect. For a horizontal and a vertical line this will only be one point, for two horizontal lines or two vertical, it could be many. However, diagonal not implemented yet."
  (let [o1 (:orientation line1)
        o2 (:orientation line2)]
    (cond
      (and (not= o1 o2) (not= o1 :diagonal) (not= o2 :diagonal)) (comp-one-point line1 line2) ; only if h and v lines for now
      :else nil)))

