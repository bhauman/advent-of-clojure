(ns advent.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.set :refer [union]]
   [clojure.math.combinatorics :refer [permutations]]))

(def prob13
  (line-seq (io/reader (io/resource "prob13"))))

(defn line-to-data [line]
  (as-> line x
    (string/replace x "." "")
    (read-string (str "(" x ")"))
    ((juxt (comp set (juxt first last))
           #((if (= (nth % 2) 'lose) - +) (nth % 3))) x)
    (apply hash-map x)))

(def seating-pair-values (apply merge-with + (map line-to-data prob13)))

(def guest-names (reduce union (keys seating-pair-values)))

(defn arrangement-value [pair-values order]
  (reduce +
          (map (comp pair-values set)
               (partition 2 1 (cons (last order) order)))))

;; part 1
#_(reduce max (map (partial arrangement-value seating-pair-values)
                 (permutations guest-names)))

;; part 2
#_(let [pair-values (into seating-pair-values
                        (map #(vector #{'me %} 0) guest-names))]
    (reduce max (map (partial arrangement-value pair-values)
                     (permutations (cons 'me guest-names)))))
