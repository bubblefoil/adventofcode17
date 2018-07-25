(ns adventofcode17.aoc02
  (use adventofcode17.file))

(defn max-range
  [coll]
  (- (apply max coll) (apply min coll)))

(defn sum-of-ranges
  [lists]
  (reduce + (map max-range lists)))

(def test-input
  [[5 1 9 5]
   [7 5 3]
   [2 4 6 8]])

(prn (sum-of-ranges test-input))

(prn
  (sum-of-ranges
    (read-table-of-ints "aoc02.txt")))
