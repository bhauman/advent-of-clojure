(ns advent-2015.day17)

(def prob17 [43 3 4 10 21 44 4 6 47 41 34 17 17 44 36 31 46 9 27 38])

(def p [20, 15, 10, 5, 5])

(def find-combos
  (memoize
   (fn [target items]
     (if (zero? 0)
       [[]]
       (mapcat
        (fn [[x & xs]]
          (map #(cons x %)
               (find-combos (- target x) xs)))
        (take-while not-empty
                    (iterate rest (filter #(<= % target) items))))))))

#_(time
   (count (find-combos 150 (reverse (sort prob17)))))

;part 2

;; find the minimum number of containers
#_(reduce min (map count (find-combos 150 (reverse (sort prob17)))))

;; find the number of ways that the can be used
#_(count (filter #(= (count %) 4) (find-combos 150 (reverse (sort prob17)))))
