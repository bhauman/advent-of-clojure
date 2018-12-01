(ns advent-2015.day07
  (:require
   [clojure.java.io :as io]))

(def prob7
  (line-seq (io/reader (io/resource "2015/prob7"))))

(defn dependents [op command]
  (filter (complement integer?)
          (condp = op
            'NOT    (rest command)
            'OR     ((juxt first last) command)
            'AND    ((juxt first last) command)
            'LSHIFT (list (first command))
            'RSHIFT (list (first command))
            command)))

(defn parse-connection [line]
  (let [tokens (read-string (str "[" line "]"))
        output (last tokens)
        command (take-while #(not= '-> %) tokens)
        op (first (filter #(and
                            (not (integer? %))
                            (re-matches #"[A-Z]+" (name %))) command))]
    {:orig tokens
     :output output
     :command command
     :deps    (dependents op command)
     :op op}))

(def source (map parse-connection prob7))

(def source-graph (into {} (map (juxt :output identity) source) ))

(defn constant-assignment? [{:keys [op command]}]
  (and
   (nil? op)
   (= 1 (count command))
   (integer? (first command))))

(defn solvable? [source-graph {:keys [deps] :as node}]
  (or
   (and
    (not-empty deps)
    (every? (comp integer? source-graph) deps))
   (constant-assignment? node)))

(defn value [source-graph k]
  (if (integer? k) k (source-graph k)))

(defn solve [source-graph {:keys [op deps command] :as node}]
  (if-let [sol (if (constant-assignment? node)
                 (first command)
                 (let [bin-args (map (partial value source-graph)
                                     ((juxt first last) command))]
                   (condp = op
                     'NOT    (bit-not (value source-graph (last command)))
                     'OR     (apply bit-or bin-args)
                     'AND    (apply bit-and bin-args)
                     'LSHIFT (apply bit-shift-left bin-args)
                     'RSHIFT (apply bit-shift-right bin-args)
                     nil     (value source-graph (first command)))))]
    (assoc source-graph (:output node) sol)
    source-graph))

(defn update-step [source-graph]
  (->> source-graph
       vals
       (filter (partial solvable? source-graph))
       (reduce solve source-graph)))

; part 1
#_((->> source-graph
     (iterate update-step)
     (filter #(integer? (% 'a)))
     first)
   'a)

;; part 2
#_((->> (assoc source-graph 'b 3176)
        (iterate update-step)
        (filter #(integer? (% 'a)))
        first)
 'a)
