(ns advent-2015.day16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def prob16
  (line-seq (io/reader (io/resource "2015/prob16"))))

(defn read-grandma [line]
  (as-> line x
    (string/replace x ":" "")
    (read-string (str "(" x ")"))
    (map #(if (symbol? %) (keyword %) %) x)
    (apply hash-map x)))

(def grandmas (map read-grandma prob16))

(defn dist* [a b] (java.lang.Math/abs (- a b)))

(def clues
  {:children 3
   :cats 7
   :samoyeds 2
   :pomeranians 3
   :akitas 0
   :vizslas 0
   :goldfish 5
   :trees 3
   :cars 2
   :perfumes 1})

(def clues-dist-map
  (zipmap (keys clues)
          (map #(partial dist* %) (vals clues))))

(defn distance [clues-dist-map grandma]
  (->> grandma
      (filter #(not= :Sue (first %)) )
      (map (fn [[k v]] ((clues-dist-map k) v)) )
      (apply +)))

;; part 1
#_(first (sort-by second (map (juxt :Sue (partial distance clues-dist-map)) grandmas)))

(defn lt-dist [a b]
  (if (< a b)
    0
    (dist* (inc a) b)))

(defn gt-dist [a b]
  (if (> a b)
    0
    (dist* a (inc b))))

(def new-dist-map
  (assoc clues-dist-map
         :cats        #(gt-dist % 7)
         :trees       #(gt-dist % 3)
         :pomeranians #(lt-dist % 3)
         :goldfish    #(lt-dist % 5)))

; part 2
#_(first (sort-by second (map (juxt :Sue (partial distance new-dist-map)) grandmas)))
