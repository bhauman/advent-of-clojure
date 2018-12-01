(ns advent-2015.day10)

(defn count-encode [x]
  (apply str
         (mapcat 
          (juxt count first)
          (partition-by identity x))))

;; part 1 and part 2
#_(count (nth (iterate count-encode "1321131112") 40 #_50))
