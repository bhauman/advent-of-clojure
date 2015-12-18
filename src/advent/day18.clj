(ns advent.day18
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [intersection union]]))

(def prob18
  (line-seq (io/reader (io/resource "prob18"))))

(defn create-board [lines]
  (into #{}
        (mapcat
         (fn [y line]
           (keep-indexed #(when (= \# %2) [%1 y]) line))
         (range 100)
         lines)))

(def offsets [[0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]])

(def get-neighbors
  (memoize
   (fn [location]
     (into #{} (filter #(every? (fn [x] (<= 0 x 99)) %)
                       (map #(map + % location) offsets))))))

(defn count-neighbors [board location]
  (count (intersection board (get-neighbors location))))

(defn cell-value [board location]
  (let [on?          (board location)
        neighbors-on (count-neighbors board location)]
    (cond
      (and on? (<= 2 neighbors-on 3))    location
      (and (not on?) (= neighbors-on 3)) location
      :else nil)))

(defn next-board [cell-value-fn board]
  (into #{} (keep (partial cell-value-fn board)
                  (for [x (range 100) y (range 100)] [x y]))))

#_(count (nth (iterate (partial next-board cell-value)
                       (create-board prob18))
              100))

(def always-on #{[0 0] [0 99] [99 0] [99 99]})

(defn part-2-cell-value [board location]
  (if (always-on location)
    location
    (cell-value board location)))

#_(count (nth (iterate (partial next-board part-2-cell-value)
                       (union (create-board prob18) always-on) )
              100))
