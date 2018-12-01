(ns advent-2015.day09
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [union]]   
   [clojure.math.combinatorics :as combo]))

(def prob9
  (line-seq (io/reader (io/resource "2015/prob9"))))

(defn tokens [x] (read-string (str "(" x ")") ))

(def dist-map
  (into {} (map (comp (fn [[a _ b _ c]] [#{a b} c])
                      tokens)
                prob9)))

(def towns (reduce union (keys dist-map)))

  ;; part 1
#_(reduce min #_max ; part 2 
          (map
           #(reduce + (map (comp dist-map set)
                           (partition 2 1 %)))
           (combo/permutations towns)))
