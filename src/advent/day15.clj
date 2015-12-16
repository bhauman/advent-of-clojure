(ns advent.day15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.math.combinatorics :refer [selections combinations]]))

(def prob15
  (line-seq (io/reader (io/resource "prob15"))))

(defn read-ingredient [line]
  (as-> line x
    (string/replace x ":" "")
    (read-string (str "(" x ")"))
    (filter integer? x)))

(def ingredients (map read-ingredient prob15))

(defn ingredient-value [ingred tsp]
  (map #(* tsp %) (butlast ingred)))

(defn recipe-value [parts]
  (->> parts
       (map ingredient-value ingredients)
       (reduce #(map + %1 %2))
       (map #(max 0 %))
       (reduce *)))

(defn partitions [i]
  (for [x (range (inc i))
        y (range (inc (- i x)))
        z (range (inc (- i x y)))]
    [x y z (- i x y z)]))

#_(def res (mapv (juxt identity recipe-value) (partitions 100)))

;; part 1 
#_(apply max (map second res))

;; part 2
(defn calorie-value [parts]
  (reduce + (map * parts (map last ingredients))))

#_(reduce max (map second (filter #(= 500 (calorie-value (first %))) res)))
