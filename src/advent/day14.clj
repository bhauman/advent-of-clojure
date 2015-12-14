(ns advent.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def prob14 (line-seq (io/reader (io/resource "prob14"))))

(defn line-to-data [line]
  (as-> line x
    (string/replace x "." "")
    (read-string (str "(" x ")"))
    (let [n (first x)
          [speed travel restt] (filter integer? x)]
      {:nm n :speed speed :travel travel :restt restt})))

(def reindeer (map line-to-data prob14))

(def test-reindeer
  [{:nm 'Comet  :speed 14 :travel 10 :restt 127}
   {:nm 'Dancer :speed 16 :travel 11 :restt 162}])

(defn traveled [{:keys [speed travel restt]} seconds]
  (let [period (+ travel restt)]
    (+ (* travel speed (int (/ seconds period)))
     (let [finalp (rem seconds period)]
       (if (> travel finalp)
         (* speed finalp)
         (* speed travel))))))

; part 1
#_(reduce max (map #(traveled % 2503) reindeer))

(defn winners [reindeer seconds]
  (let [results (map
                 #(assoc % :distance (traveled % seconds))
                 reindeer)
        winner-score (apply max (map :distance results))]
    (map :nm
         (filter #(= (:distance %) winner-score) results))))

; part 2
#_(reduce max
 (vals
  (frequencies
   (mapcat (partial winners reindeer)
           (range 1 (inc 2503))))))
