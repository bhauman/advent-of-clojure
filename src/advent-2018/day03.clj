(ns advent-2018.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (->> (line-seq (io/reader (io/resource "2018/day03")))
       (map #(->> %
                  (re-seq #"\d+")
                  (map read-string)))))

(defn square [m [id x y w h]]
  (reduce
   #(update %1 %2 (fn [p] (if p 'X id)))
   m
   (for [x' (range x (+ x w))
         y' (range y (+ y h))]
     [x' y'])))

(def fabric
  (memoize
   (fn [data]
     (reduce square {} data))))

(defn part1 [d]
  (count (filter #(= 'X %) (vals (fabric d)))))

#_ (time  (part1 data))

(defn part2 [d cloth]
  (let [actual-area-map (frequencies (vals cloth))
        ideal-area-map
        (into {} (map (fn [[id _ _ w h]] [id (* w h)])) d)]
    (->> (keys actual-area-map)
         (filter #(= (get actual-area-map %)
                     (get ideal-area-map %)))
         first)))

#_ (time (part2 data (fabric data)))
