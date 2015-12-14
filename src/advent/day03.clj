(ns advent.day03
  (:require
   [clojure.java.io :as io]))

(def prob3 (slurp (io/resource "prob3"))) 

(defn positions-visited [data]
  (set
   (reductions
    #(map + %1 %2)
    [0 0]
    (map {\^ [0 1]
          \v [0 -1]
          \> [1 0]
          \< [-1 0]} data))))

;; part 1
#_(count
    (positions-visited prob3))

;; part 2

#_(count
   (set
    (mapcat positions-visited
            (let [p (partition 2 prob3)]
              [(map first p) (map second p)]))))
