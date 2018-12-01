(ns advent-2015.day01
  (:require
   [clojure.java.io :as io]))

(def prob1 (slurp (io/resource "2015/prob1")))

;; part 1
#_(reduce + (map {\( 1 \) -1} prob1))

;; part 2
#_(count
   (take-while #(not= % -1)
               (reductions + 0 (map {\( 1 \) -1} prob1))))
