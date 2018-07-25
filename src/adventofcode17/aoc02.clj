(ns adventofcode17.aoc02
  (use adventofcode17.file)
  (:require [clojure.math.combinatorics :as combo]))

(prn "Part One")

(defn max-range
  [coll]
  (- (apply max coll) (apply min coll)))

(defn sum-of-ranges
  [lists]
  (reduce + (map max-range lists)))

(def input-1 [[5 1 9 5]
              [7 5 3]
              [2 4 6 8]])

(prn (sum-of-ranges input-1))

(prn
  (sum-of-ranges
    (read-table-of-ints "aoc02.txt")))

;---------------------------
(prn "Part Two")

(def input-2 [[5 9 2 8]
              [9 4 7 3]
              [3 8 6 5]])

(defn all-pairs [coll]
  (combo/combinations coll 2))

(defn ratios [x y]
  (vector (/ x y) (/ y x)))

(defn find-evenly-divisible
  [coll]
  (->> coll
       (all-pairs)
       (mapcat #(apply ratios %))
       (filter integer?)
       (first)))

(assert (= 4 (find-evenly-divisible [3 2 5 8])))

(defn sum-of-even-divisions [colls]
  (->> colls
       (map find-evenly-divisible)
       (reduce +)))

(prn (sum-of-even-divisions input-2))

(prn
  (sum-of-even-divisions
    (read-table-of-ints "aoc02.txt")))
