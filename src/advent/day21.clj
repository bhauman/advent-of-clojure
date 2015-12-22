(ns advent.day21
  (:require
   [clojure.math.combinatorics :as combo]))

(defn read-table [table-str]
  (partition 4 (read-string (str "(" table-str ")"))))

(def weapons
  (read-table
     "Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0"))

(def armor
  (read-table
     "None          0     0       0
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5"))

(def rings
  (read-table
   "None  0 0 0
Damage+1    25     1       0
Damage+2    50     2       0
Damage+3   100     3       0
Defense+1   20     0       1
Defense+2   40     0       2
Defense+3   80     0       3"))

(def ring-combos
  (cons [(first rings) (first rings)]
        (combo/combinations rings 2)))

(defn cost [things] (reduce + (map second things)))

(defn value [things] (reduce + (mapcat #(nthrest % 2) things)))

(def choices
  (map #(concat (take 2 %) (last %))
       (combo/cartesian-product weapons armor ring-combos)))

(defn your-player-wins? [total]
  (->> (reductions  #(map - %1 %2) [109 100] (cycle [[total 0] [0 10]]))
       (filter #(some (partial >= 0) %))
       first
       second
       pos?))

;; part 1
#_(your-player-wins? 11) ;; you win if total damage armor is 11

;; find minimum cost for a total of 11
#_(first (sort-by second (filter #(= 11 (first %)) (map (juxt value cost identity) choices))))


;; part 2
#_(your-player-wins? 10) ;; you lose if total damage armor is 10

;; find maximum cost for a total of 10
#_(last (sort-by second (filter #(= 10 (first %)) (map (juxt value cost identity) choices))))
