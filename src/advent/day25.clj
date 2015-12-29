(ns advent.day25)

; Enter the code at row 2947, column 3029.

(defn pos-from-row-col [r c]
  (+ (nth
      (reductions + (range))
      (dec (+ c (dec r))))
     c))

(defn at-pos [pos]
  (loop [x 20151125
         c (dec pos)]
    (if (zero? c)
      x
      (recur (rem (* x 252533) 33554393)
             (dec c)))))

#_(at-pos (pos-from-row-col 2947 3029))
