(ns advent.core
  (:require [clojure.java.io :as io]))

(def prog
  (map #(read-string (str "(" % ")"))
       (line-seq (io/reader (io/resource "prob23")))))  

(defn run [prog start-state]
  (loop [{:keys [pos] :as st} start-state]
    (if (not (< pos (count prog)))
      st
      (recur
       (let [[cmd sec :as inst] (nth prog pos)
              reg-key (when (symbol? sec) sec)]
         (condp = cmd
           'jio (if (= (st reg-key) 1)
                  (update-in st [:pos] + (last inst))
                  (update-in st [:pos] inc))
           'jie (if (even? (st reg-key))
                  (update-in st [:pos] + (last inst))
                  (update-in st [:pos] inc))
           'jmp (-> st
                    (update-in [:pos] + (last inst)))
           'inc (-> st
                    (update-in [reg-key] inc)
                    (update-in [:pos] inc))
           'tpl (-> st
                    (update-in [reg-key] * 3)
                    (update-in [:pos] inc))
           'hlf (-> st
                    (update-in [reg-key] #(int (/ % 2)))
                    (update-in [:pos] inc))))))))

#_(run prog {'a 0 'b 0 :pos 0})

#_(run prog {'a 1 'b 0 :pos 0})
