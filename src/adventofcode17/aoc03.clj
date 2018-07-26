(ns adventofcode17.aoc03)

(defn perimeters []
  "Sequence of square perimeter lengths."
  (iterate #(+ % 8) 0))

(defn layer-ends []
  "Sequence of the last numbers of the square spiral layers."
  (conj
    (->> (perimeters)
         (reductions +)
         (map inc))
    0))

(defn layer-ranges []
  (partition 2 1 (layer-ends)))

(defn layers-with [x]
  (take-while #(> x (first %)) (layer-ranges)))

;(prn (layers-with 1023))

(defn distance-to-side-center [a b x]
  (assert (<= a x b))
  (let [perimeter (- b a)
        side (/ perimeter 4)
        spiral-distance (mod (- x a) side)
        side-center-distance (long (- spiral-distance (/ side 2)))]
    ;(prn a b x "->" perimeter spiral-distance side-center-distance)
    (Math/abs side-center-distance)))

(assert (= (distance-to-side-center 9 25 12) 1))
(assert (= (distance-to-side-center 9 25 16) 1))
(assert (= (distance-to-side-center 9 25 21) 2))
(assert (= (distance-to-side-center 1 9 2) 0))
(assert (= (distance-to-side-center 1 9 4) 0))
(assert (= (distance-to-side-center 1 9 5) 1))

(defn manhattan [x]
  (let [layers-with-x (layers-with x)
        layer-with-x (last layers-with-x)
        layer-count (dec (count layers-with-x))
        perimeter-distance (distance-to-side-center (first layer-with-x) (last layer-with-x) x)]
    ;(prn "layer: " layer-count "perimeter: " perimeter-distance)
    (+ layer-count perimeter-distance)))

(assert (= (manhattan 1) 0))
(assert (= (manhattan 12) 3))
(assert (= (manhattan 23) 2))
(assert (= (manhattan 1024) 31))

(prn (manhattan 361527))