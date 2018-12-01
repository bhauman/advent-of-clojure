(ns advent-2015.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def prob2
  (line-seq (io/reader (io/resource "2015/prob2"))))

(defn parse-dims [line]
  (map read-string (string/split line #"x")))

(def dimensions (map parse-dims prob2))

(defn side-dims [dims]
  (partition 2 1 (conj (vec dims) (first dims))))

(defn calc-area-of-present [dims]
  (as-> dims x
    (side-dims x)
    (map #(apply * %) x)
    (reduce + (reduce min x)
            (map #(* 2 %) x))))

;; part 1
#_(reduce + (map calc-area-of-present dimensions)) 

(defn calc-shortest-perimiter [dims]
  (as-> dims x
    (side-dims x)
    (first (sort-by #(apply * %) x))
    (+ (apply * dims)
       (* 2 (apply + x)))))

;; part 2
#_(reduce + (map calc-shortest-perimiter dimensions))
