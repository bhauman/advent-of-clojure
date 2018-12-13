(ns advent-2018.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [medley.core :as med]))

(defn parse [s]
  (->> (str "(" s ")")
   read-string
   (partition 2)))

(def data (parse (slurp (io/resource "2018/day06"))))

(def example (parse
              "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"))

(def min-max (juxt #(apply min %) #(apply max %)))

(defn bounds [points]
  (map vector
       (min-max (map first points))
       (min-max (map second points))))

(defn distance [[x y] [a b]]
  (+ (Math/abs (- x a))
     (Math/abs (- y b))))

(defn edge-positions [[x y] [x1 y1]]
  (distinct
   (concat (map vector
                (range x x1)
                (repeat y))
           (map vector
                (range x x1)
                (repeat y1))
           (map vector
                (repeat x)
                (range y y1))
           (map vector
                (repeat x)
                (range y y1)))))

(defn closest-point-or-tie [points point]
  (let [res (sort-by second (map (juxt identity #(distance point %))
                                 points))]
    (if (apply = (map second (take 2 res)))
      :tie
      (ffirst res))))

(defn closest-map [nodes positions]
  (reduce (fn [accum p]
            (assoc accum p (closest-point-or-tie nodes p)))
          {} positions))

(defn solve-1 [nodes]
  (let [[[x1 y1] [x2 y2]] (bounds nodes)
        positions (combo/cartesian-product
                   (range x1 (inc x2))
                   (range y1 (inc y2)))
        edge-points
        (edge-positions [(- x1 5) (- y1 6)]
                        [(+ x2 5) (+ y2 6)])
        result (closest-map nodes positions)
        edge-result (closest-map nodes edge-points)]
    (second
     (first (sort-by 
             second >
             (apply dissoc
                    (->> result vals frequencies)
                    (-> edge-result vals set)))))))

;; part 1
#_(time (solve-1 data))

(defn distance-to-all [point points]
  {:pre [point points]}
  (apply + (map (partial distance point) points)))

(def directions [[0 -1] [0 1] [-1 0] [1 0]])

(defn direction-search [valid? seen-set next-set]
  (if (empty? next-set)
    seen-set
    (let [point (first next-set)]
      (if (and (not (seen-set point))
               (valid? point))
        (recur valid?
               (conj seen-set point)
               (into next-set
                     (filter
                      (complement seen-set)
                      (mapv #(map + point %1)
                            directions))))
        (recur valid? seen-set (disj next-set point))))))

(defn solve2 [limit d]
  (let [pred? #(< (distance-to-all % d) limit)]
    (direction-search pred? #{}
                      #{(first (filter pred? d))})))

;; part 2
#_(time (count (solve2 10000 data)))







