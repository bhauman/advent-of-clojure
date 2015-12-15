(ns advent.day14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def prob14 (line-seq (io/reader (io/resource "prob14"))))

(defn line-to-data [line]
  (->> line
       (re-seq #"\d+")
       (map read-string)))

(def reindeer (map line-to-data prob14))

(defn traveled [[speed travel restt] seconds]
  (let [period (+ travel restt)]
    (+ (* travel speed (int (/ seconds period)))
     (let [finalp (rem seconds period)]
       (if (> travel finalp)
         (* speed finalp)
         (* speed travel))))))

; part 1
#_(reduce max (map #(traveled % 2503) reindeer))

(defn winners [reindeer seconds]
  (let [res (apply merge-with concat
                   (map
                    #(hash-map (traveled % seconds) [%])
                    reindeer))]
    (get res (apply max (keys res)))))

; part 2
#_(reduce max
          (vals
           (frequencies
            (mapcat (partial winners reindeer)
                    (range 1 (inc 2503))))))
