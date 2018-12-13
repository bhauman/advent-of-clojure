(ns advent-2018.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as med]))

#_(remove-ns 'advent-2018.day07)

(defn parse [s]
  (->> (string/split-lines s)
       (mapv #(vec (map (comp char first) (rest (re-seq #"[A-Z]" %)))))))

(def ex (parse "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."))

(def data (parse (slurp (io/resource "2018/day07"))))

(defn relation-map [d]
  (apply
   merge-with
   conj
   (into {} (map (juxt first (constantly #{}))) d)
   (map #(apply hash-map %) d)))

(def parents relation-map)

(defn children [d] (relation-map (map reverse d)))

(defn roots [data]
  (let [cm (children data)]
    (into (sorted-set-by #(< (int %1) (int %2)))
          (filter (complement cm)
                  (distinct (flatten data))))))

(defn order [parents prereqs accum stack]
  (if (empty? stack)
    accum
    (let [next (->> stack
                    (filter #(empty? (reduce disj (prereqs %) accum)))
                    first)
          accum (conj accum next)]
      (recur parents prereqs accum
             (reduce disj (into stack (parents next)) accum)))))

(defn solve-1 [d]
  (apply str (order (parents d) (children d) [] (set (roots d)))))

#_(= (solve-1 data)
     "CHILFNMORYKGAQXUVBZPSJWDET")

(def ^:dynamic *slots* 2)
(def ^:dynamic *seconds-offset* 0)

(defn seconds [c]
  (+ (- (int c) 64)
     *seconds-offset*))

(defn work-second [{:keys [prereqs available completed workers] :as state}]
  (let [next-workers   (med/map-vals dec workers)
        complete       (keys (med/filter-vals #(zero? %) next-workers))
        next-workers   (reduce dissoc next-workers complete)
        next-completed (concat completed complete)
        next-ones (take (- *slots* (count next-workers))
                        (filter
                         #(empty? (reduce disj (prereqs %) next-completed))
                         available))]
    (assoc state
           :available (remove (set next-ones) available)
           :completed next-completed
           :workers (reduce #(assoc %1 %2 (seconds %2)) next-workers
                             next-ones))))

(defn solve-2 [n-workers offset data]
  (binding [*slots* n-workers
            *seconds-offset* offset]
    (count (take-while
            (comp not-empty :workers)
            (rest (iterate work-second
                           {:prereqs (children data)
                            :available (solve-1 data)
                            :completed []
                            :workers {}}))))))

#_(solve-2 5 60 data)




