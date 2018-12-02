(ns advent-2018.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.set :as set]
   [medley.core :as med]
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

#_ (time (problem-2 data))

(def test-data-2
  (map name '[abcde
              fghij
              klmno
              pqrst
              fguij
              axcye
              wvxyz]))

;; faster efficient tree search
(defn search [words]
  (let [grouped (med/map-vals (partial map rest) (group-by first words))]
    (swap! steps inc)
    (or
     (and (< 1 (count grouped))
          (->> (vals grouped)
               (map set)
               (apply set/intersection)
               first))
      (some->> grouped
               (med/filter-vals #(< 1 (count %)))
               (med/map-vals search)
               (med/filter-vals some?)
               first
               (apply cons)))))

#_(time (apply str (search data)))






