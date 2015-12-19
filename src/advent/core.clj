(ns advent.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.core.match :refer [match]]
   [clojure.pprint :as p]
   [clojure.set :refer [union intersection difference]]
   [clojure.math.combinatorics :as combo]
   [clojure.walk :refer [prewalk postwalk]]
   [digest :refer [md5]]))

;; workarea
(def rule-lines
  (line-seq (io/reader (io/resource "prob19.rules"))))

(defn parse-rule [x]
  (map (comp seq name)
       ((juxt first last)
        (read-string (str "(" x ")")) )))

(def rules (map parse-rule rule-lines))

(def molecule (slurp (io/resource "prob19.mol")))

(defn rule-replacements [[match replace :as rule] s]
  (if (empty? s) []
      (let [replacements (map #(cons (first s) %)
                              (rule-replacements rule (rest s)))]
        (if (= match (take (count match) s))
          (cons (concat replace (drop (count match) s)) replacements)
          replacements))))
; part 1
(count (set (mapcat #(rule-replacements % molecule)
                    rules)))

;; invert rule operation
(defn rule-reductions [[match replace :as rule] s]
  (if (empty? s) []
      (let [replacements (map #(cons (first s) %)
                              (rule-reductions rule (rest s)))]
        (if (= replace (take (count replace) s))
          (cons (concat match (drop (count replace) s)) replacements)
          replacements))))

(defn narrow-field-by [n strs]
  (let [groups (group-by count strs)
        lengths (take n (sort (keys groups)))]
    (mapcat groups lengths)))

(defn reduce-step [strs]
  (into #{}
   (mapcat
    #(mapcat (fn [rule] (rule-reductions rule %))
             rules)
    (narrow-field-by 1 strs)))

  ;; could sort by length and take the shortest ones
  )

#_(time
   (sort (keys (group-by count
                         (nth (iterate reduce-step #{molecule}) 20)))))


#_(count
   (take-while
    #(not (% [\e]))
    (iterate reduce-step #{molecule})))

