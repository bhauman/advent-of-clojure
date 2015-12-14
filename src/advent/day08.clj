(ns advent.day08
  (:require
   [clojure.java.io :as io]))

(def prob8
  (line-seq (io/reader (io/resource "prob8"))))

(defn unescape-trunc [input]
  (drop (condp = (take 2 input)
          [\\ \"] 2 
          [\\ \\] 2 
          [\\ \x] 4 
          1)
        input))

(defn unescape-len [x]
  (+ -2
     (count (take-while not-empty
                        (iterate unescape-trunc x)))))

;; part 1
#_(reduce + 0 (map #(- (count %) (unescape-len %)) prob8))

(defn expand-len [w]
  (reduce + 2
          (map #(condp = %
                  \" 2
                  \\ 2
                  1)
               w)))

;; part 2
#_(reduce + 0 (map #(- (expand-len %) (count %)) prob8))
