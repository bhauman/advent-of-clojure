(ns advent.prob15
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

;; workarea

(def prob15
  (line-seq (io/reader (io/resource "prob15"))))

(defn read-line [line]
  (as-> line x
    (string/replace x ":" "")
    (read-string (str "(" x ")"))
    (filter integer? x)))

(def ingredients (map read-line prob15))

(defn ingredient-value [ingred tsp]
  (map #(* tsp %) (butlast ingred)))

(defn combo-value [parts]
  (reduce *
          (map #(max 0 %)
               (reduce #(map + %1 %2) (map ingredient-value ingredients parts)))))

(def partition-number
  (memoize (fn [parts i]
             (cond
               (= 1 parts) [[i]] 
               :else (mapcat
                      #(map (fn [x] (cons % x)) (partition-number (dec parts) (- i %)))
                      (range (inc i)))))))

#_(def res (mapv (juxt identity combo-value) (partition-number 4 100)))

;; part 1 
#_(apply max (map second res))

;; part 2

(defn calorie-value [parts]
  (reduce + (map * parts (map last ingredients))))

#_(reduce max (map second (filter #(= 500 (calorie-value (first %))) res)))
