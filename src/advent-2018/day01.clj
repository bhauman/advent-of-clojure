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
  (->> (cycle d)
       (reductions (fn [[i a b] v]
                     (let [sum (+ a v)]
                       [(inc i) sum (conj b sum)]))
                   [0 0 #{0}])
       (filter (fn [[index _ sums]] (not= index (dec (count sums)))))
       first
       second))

#_(part2 data)
