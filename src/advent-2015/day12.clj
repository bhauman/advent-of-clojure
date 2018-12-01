(ns advent-2015.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.walk :refer [prewalk]]))

(def prob12 (slurp (io/resource "2015/prob12")))

;; part 1
#_(reduce + (map read-string (re-seq #"[-]{0,1}\d+" prob12)))

;; part 2

(defn has-red? [m]
  (some (fn [me] ((set me) "red")) m))

#_(as-> prob12 a
    (string/replace a ":" " ")
    (read-string a)           
    (prewalk #(cond
                (and (map? %) (has-red? %)) nil
                (map? %) (seq %)
                :else %) a)
    (flatten a)
    (filter integer? a)
    (reduce + a))
