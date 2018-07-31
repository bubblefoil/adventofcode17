(ns adventofcode17.aoc03b
  (:require [clojure.math.numeric-tower :as m]))

; Part 2 not working yet.

(defn perimeter [radius]
  "Perimeter of a spiral layer based on the distance from the center."
  (* radius 8))

(defn perimeter-increment [p]
  "The difference between arc distances on"
  (let [spiral-dist (* 4 (/ (:s p) (perimeter (:r p))))]
    (if (integer? spiral-dist)                              ;in the case of corners
      (* 2 spiral-dist)
      (-> spiral-dist
          (int)
          (* 2)
          (inc)))))

(defn inner [p]
  (if (= 0 (:r p))
    {:r 0 :s 0}
    (merge-with -
                p
                {:r 1 :s (perimeter-increment p)})))

(defn outer [p]
  (merge-with +
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
  (condp = (:s p)
    0 {:r 0 :s 0}
    1 {:r (dec (:r p)) :s (perimeter (dec (:r p)))}
    (merge-with -
                p
                {:s 1})))

(defn visited-neighbors [p]
  (let [n (condp = (:r p)
            0 []
            1 (vector (previous (previous p)))
            (vector (outer (previous (inner p)))))]
    (distinct
      (conj n
            (previous p)
            (inner (following p))
            (inner p)
            (inner (previous p))))))

(def sum-of-neighbors2
  (memoize (fn [p]
             (if (= 0 (:r p))
               1
               (reduce +
                       (map sum-of-neighbors2
                            (visited-neighbors p)))))))

(defn spiral []
  (iterate following {:r 0 :s 0}))

(defn abs-limit [x lim]
  (min lim (max x (- lim))))

(->> (drop 9 (take 25 (spiral)))
     (map #(merge %
                  {:x (abs-limit (- (mod (:s %) (* 4 (:r %))) 4) (:r %))
                   :y (abs-limit (- (mod (+ 4 (:s %)) (* 4 (:r %))) 4) (:r %))})))

;    |-3 -2 -1 0  1  2  3
;  4 |-------------------
;  3 |12 11 10 9  8  7  6
;  2 |13 8  7  6  5  4  5
;  1 |14 9  4  3  2  3  4
; -0 |15 10 5  0  1  2  3
; -1 |16 11 6  7  8  1  2
; -2 |17 12 13 14 15 16 1
; -3 |18 19 20 21 22 23 24 1
; -4 |----------------------
;    |-3 -2 -1 0  1  2  3
