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
#_(count (set (mapcat #(rule-replacements % molecule)
                    rules)))

(defn apply-rule [[match replace] s]
  (let [n (count replace)]
    (when (= replace (take n s))
      (concat match (drop n s)))))

(def rule-reductions
  (memoize
   (fn self [s]
     (if (empty? s) []
         (let [replacements (map #(cons (first s) %)
                                 (self (rest s)))]
           (if-let [rule-results (not-empty (keep #(apply-rule % s) (take 40 rules)))]
             (concat rule-results replacements)
             replacements))))))

(def test-rules
  (map parse-rule
       (string/split "e => H
e => O
H => HO
H => OH
O => HH"
#"\n" )))

(count (time (rule-reductions molecule)))

(def narrow-field-by
  (fn [n strs]
    (let [groups (group-by count strs)
          lengths (take n (sort (keys groups)))
          result (mapcat groups lengths)]
      (println lengths)
      (println (count result))
      result)))

(defn reduce-step [strs]
  (into #{}
        (apply concat
               (pmap
                #(rule-reductions %)
                (narrow-field-by 1 strs))))

  ;; could sort by length and take the shortest ones
  )

#_(time
   (sort (keys (group-by count
                         (nth (iterate reduce-step #{molecule}) 20)))))


#_(count
   (take-while
    #(not (% [\e]))
    (iterate reduce-step #{molecule})))

