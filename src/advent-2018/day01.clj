(ns advent-2018.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (->> (io/resource "2018/day01")
      slurp
      (format "(%s)")
      read-string))

;; part 1
(def part1 (partial reduce +))

#_ (= (part1 data) 510)

;; part 2
(defn part2 [d]
  (reduce
   #(if (%1 %2) (reduced %2) (conj %1 %2))
   #{0}
   (reductions + (cycle d))))

#_(part2 data)
