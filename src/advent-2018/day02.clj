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

;; more efficient tree search
(defn search [words]
  (when (< 1 (count words))
    ;; separate into groups whos first letters differ from the other groups
    (let [grouped (med/map-vals (partial map #(subs % 1))
                                (group-by first words))]
      ;;two cases
      (or
       ;; everything upto now is equal except for the current letters
       ;; check to see if any words are equal after this point
       (->> (combo/combinations (map set (vals grouped)) 2)
            (keep #(not-empty (apply set/intersection %)))
            ffirst)
       ;; the search is now divided between words that have equal beginnings
       (some->> grouped
                (med/map-vals search)
                (med/filter-vals some?)
                first
                (apply cons))))))

#_(= (apply str (search data)) (problem-2 data))

#_ (time (apply str (search data)))
;; 5 ms

#_(time (problem-2 data))
;; 90ms

#_(search ["abc" "ddd" "zbc" "ahg" "zzz"])



