(ns advent.day19
  (:require
   [clojure.java.io :as io]))

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

(def reversed-rules
  (map #(map reverse %) rules)) 

(defn apply-rule [[match replace] s]
  (let [n (count replace)]
    (when (= replace (take n s))
      (concat match (drop n s)))))

(defn first-rule-reduce [rules s]
  (if (empty? s) nil
      (if-let [result (first (keep #(apply-rule % s) rules))]
        result
        (when-let [res (first-rule-reduce rules (rest s))]
          (cons (first s) res)))))

; part 2
; this is set up to be solved in reverse easily
; discovered this by reading the adventofcode reddit forum

(count (take-while #(not= % [\e])
            (iterate (partial first-rule-reduce reversed-rules)
                     (reverse molecule))))
