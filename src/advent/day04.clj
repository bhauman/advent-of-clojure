(ns advent.day04
  (:require
   [clojure.java.io :as io]
   [digest :refer [md5]]))

;; day 4 

;; part 1
#_(first
   (filter
    #(= "00000" #_ "000000" (subs (second %) 0 5 #_6))
    (map
     (juxt identity md5)
     (map-indexed #(str %2 %1) (repeat "yzbqklnj")))))

;; part 2  change the parameters above
