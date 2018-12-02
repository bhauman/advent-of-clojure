(ns advent-2018.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]))

(def data
  (->> (io/resource "2018/day02")
       slurp
       string/split-lines))

(defn occurs [str]
  (set (vals (frequencies str))))

(def transpose (partial apply mapv vector))

(defn problem-1 [d]
  (->> (map (comp (juxt #(when (% 2) 1) #(when (% 3) 1)) occurs) d)
       transpose
       (map #(apply + (keep identity %)))
       (apply *)))

#_ (problem-1 data)

(defn comparison [x y]
  (filter identity (map #(when (= %1 %2) %1) x y)))

(defn problem-2 [d]
  (let [target (dec (count (first d)))]
    (->> (combo/combinations d 2)
         (map #(apply comparison %))
         (filter #(= (count %) target))
         first
         (apply str))))

#_(problem-2 data)






