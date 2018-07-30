(ns adventofcode17.aoc03b
  (:require [clojure.math.numeric-tower :as m]))

(defn perimeter [radius]
  "Perimeter of a spiral layer based on the distance from the center."
  (* radius 8))

(defn in [pos]
  (let [r (nth pos 0)
        c (nth pos 1)]
    (vector (dec r) (int (Math/round (double (* c (/ (dec r) r))))))))

(defn perimeter-increment [p]
  (-> (:s p)
      (/ (perimeter (:r p)))
      (* 4)
      (int)
      (* 2)
      (inc)))

(defn inner [p]
  (merge-with -
              p
              {:r 1 :s (perimeter-increment p)}))

(defn following [p]
  (let [layer-perimeter (perimeter (:r p))]
    (if (= layer-perimeter (:s p))
      {:r (inc (:r p)) :s 1}
      (merge-with +
                  p
                  {:s 1}))))

(defn previous [p]
  (if (= 1 (:s p))
    {:r (dec (:r p)) :s (perimeter (dec (:r p)))}
    (merge-with -
                p
                {:s 1})))

(defn sur [p]
  (let [inner (inner p)]
    (set
      (vector
        (previous p)
        p
        (following p)
        (inner (previous p))
        (inner p)
        (inner (following p))))))

(defn mindfuck [p]
  (let [sum 1]
    (if (= 0 (:r p) (:s p))
      (+ sum (mindfuck (previous p)))
      sum)))

(defn next-sp2 [r s]
  (if (= s (perimeter r))
    (vector (inc r) 1)
    (vector r (inc s))))

(defn spiral []
  (iterate following {:r 0 :s 0}))

(defn turn [dir]
  (case dir
    :u :l
    :l :d
    :d :r
    :r :u))

(defn dir [r c]
  (mod (perimeter r) c))

;12 11 10 9  8  7  6
;13 8  7  6  5  4  5
;14 9  4  3  2  3  4
;15 10 5  0  1  2  3
;16 11 6  7  8  1  2
;17 12 13 14 15 16 1
;18 19 20 21 22 23 24 1

(defn distance-to-side-center [a b x]
  (assert (<= a x b))
  (let [perimeter (- b a)
        side (/ perimeter 4)
        spiral-distance (mod (- x a) side)
        side-center-distance (long (- spiral-distance (/ side 2)))]
    ;(prn a b x "->" perimeter spiral-distance side-center-distance)
    (Math/abs side-center-distance)))


;(prn (layers-with 1023))


(assert (= (distance-to-side-center 9 25 12) 1))
(assert (= (distance-to-side-center 9 25 16) 1))
(assert (= (distance-to-side-center 9 25 21) 2))
(assert (= (distance-to-side-center 1 9 2) 0))
(assert (= (distance-to-side-center 1 9 4) 0))
(assert (= (distance-to-side-center 1 9 5) 1))
;
;(defn manhattan [x]
;  (let [layers-with-x (layers-with x)
;        layer-with-x (last layers-with-x)
;        layer-count (dec (count layers-with-x))
;        perimeter-distance (distance-to-side-center (first layer-with-x) (last layer-with-x) x)]
;    ;(prn "layer: " layer-count "perimeter: " perimeter-distance)
;    (+ layer-count perimeter-distance)))
;
;(assert (= (manhattan 1) 0))
;(assert (= (manhattan 12) 3))
;(assert (= (manhattan 23) 2))
;(assert (= (manhattan 1024) 31))

;(prn (manhattan 361527))
;perimeters
;(m/exact-integer-sqrt 361527)
;(defn next-c [c]
;  (nth))