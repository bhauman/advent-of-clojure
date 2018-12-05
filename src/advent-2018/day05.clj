(ns advent-2018.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as med]
   [clj-time.core :as t]))

(def data (string/trim (slurp (io/resource "2018/day05"))))

(defn eliminate [accum [x y & xs]]
  (cond
    (nil? x) accum
    (nil? y) (cons x accum)
    (= (Math/abs (- (int x) (int y))) 32)
    (recur (rest accum) (cond-> xs
                          (not-empty accum)
                          (conj (first accum))))
    :else (recur (cons x accum) (cons y xs))))

;; part 1
#_ (count (eliminate '() data))

(defn part2 [d]
  (->> (distinct (string/lower-case d))
       (map #(do #{% (char (- (int %) 32))}))
       (map #(filter (complement %) d))
       (map #(count (eliminate '() %)))
       (reduce min)))

#_(part2 data)





