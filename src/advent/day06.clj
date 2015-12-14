(ns advent.day06
  (:require
   [clojure.java.io :as io]))

;; day 6

(def prob6
  (line-seq (io/reader (io/resource "prob6"))))

(defn parse-command [line]
  (let [x (->> (str "[" line "]")
               read-string
               (group-by integer?))]
    [(x false) (x true)]))

;; expand in to instructions for each position
(defn to-bytecode [[command [x1 y1 x2 y2]]]
  (map
   #(vector command %)
   (for [x (range x1 (inc x2))
         y (range y1 (inc y2))]
     [x y])))

(defn action [board [command loc]]
  (condp = command
    '(turn on through)  (assoc! board loc true)
    '(turn off through) (assoc! board loc false)
    '(toggle through)   (assoc! board loc (not (board loc)))))

#_(def part1 (persistent!
              (reduce action (transient {})
                      (mapcat (comp to-bytecode parse-command)
                              prob6))))

;; part 1
#_(count (filter identity (vals part1)))

(defn action2 [board [command loc]]
  (assoc! board loc
          ((condp = command
               '(turn on through)  inc
               '(turn off through) #(max (dec %) 0)
               '(toggle through)   (partial + 2))
           (or (get board loc) 0))))

#_(def part2 (persistent!
              (reduce action2 (transient {})
                      (mapcat (comp to-bytecode parse-command)
                              prob6))))
;; part 2
#_(reduce + 0 (filter identity (vals part2)))
