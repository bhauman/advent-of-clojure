(ns advent.day24
  (:require
   [clojure.math.combinatorics :as combo]))

;; workarea
(def input [1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113])

(def total (apply + input))

;; really fast way to do this
(def find-sets=
  (memoize
   (fn [g n]
     (cond
       (< n 0) nil
       (= n 0) [[]]
       :else
       (filter
        not-empty
        (mapcat
         (fn [[f & r]] (map #(cons f %) (find-sets= r (- n f))))
         (take-while not-empty (iterate rest g))))))))

;; part 1
#_(first
   (sort (map #(apply * %)
              (second (first (group-by count (find-sets= (reverse input) (/ total 3))))))))

;; part 2
#_(first
   (sort (map #(apply * %)
              (second (first (group-by count (find-sets= (reverse input) (/ total 4))))))))


;; another way is to get smallest combinations

;; part 1
(reduce min
        (map #(apply * %)
             ;; use a loop here instead of laziness
             ;; as laziness will 'look ahead' and that becomes
             ;; combinatorially problematic
             (loop [x 1]
               (let [combs (combo/combinations input x)]
                 (if-let [res (not-empty (filter #(= (/ total 3) (apply + %)) combs))]
                   res
                   (recur (inc x)))))))
