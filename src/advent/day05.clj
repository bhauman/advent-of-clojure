(ns advent.day05
  (:require
   [clojure.java.io :as io]))

(def prob5
  (line-seq (io/reader (io/resource "prob5"))))

(defn test-word [line]
  (and
   (not (re-matches #".*(ab|cd|pq|xy).*" line))
   (<= 3 (count (filter #(re-matches #".*(a|e|i|o|u).*" (str %)) line)))
   (some
    #(apply = %)
    (partition 2 1 line))))

; part 1
#_(count (filter test-word prob5))

(defn non-consecutive-pairs [word]
  (some
   (fn [[x & xs]] (some #(> (- % x) 1) xs))
   (vals
    (apply merge-with concat
           (map-indexed (fn [a b] {b [a]}) (partition 2 1 word))))))

(defn test-word2 [word]
  (and
   (non-consecutive-pairs word)
   (some (fn [[a _ b]] (= a b)) (partition 3 1 word))))

; part 2
#_(count (filter test-word2 prob5))
