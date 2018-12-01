(ns advent-2018.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (-> (io/resource "2018/day01")
      slurp
      (string/replace "+" "")
      ((partial format "(%s)"))
      read-string))

;; part 1
(def part1 (partial reduce +))

#_ (= (part1 data) 510)

;; part 2
(defn part2 [d]
  (->> (mapcat identity (repeat d))
       (reductions (fn [[a b] v]
                     (let [sum (+ a v)]
                       [sum (conj b sum)]))
                   [0 #{}])
       (map-indexed (fn [i x] (cons i x)))
       (filter (fn [[index _ sums]] (not= index (count sums))))
       first
       second
       time))

#_(part2 data)
