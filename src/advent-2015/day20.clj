(ns advent-2015.day20)

;; happy to say that I did this without looking at the forums
;; spent a decent amount of time understanding this page:
;; http://mathschallenge.net/library/number/sum_of_divisors

(defn value* [p n]
  (/ (dec (java.lang.Math/pow p (inc n)))
     (dec p)))

(defn sum-of-divisors [prime-factors]
  (apply * (map #(apply value* %) (frequencies prime-factors))))

;; I searched prime combinations by hand maximizing the (/ sum-of-divisors house-number)
;; and getting a feel for the search space
(let [d [2 2 2 2 3 3 3 5 5 7 11]
      house-number (apply * d)
      sum-of-presents (sum-of-divisors d)]
  [house-number
   sum-of-presents
   (/ sum-of-presents house-number)
   (java.lang.Math/abs (- sum-of-presents 3600000))
   (< house-number 900900)]) ;; 900900 is the last minimum I found 

;; part 2

;; brute force: I know the bounds now and understand the
;; character of the space a bit better

(defn divisors [n]
  (cons n (filter #(and
                    (>= (* 50 %) n)
                    (zero? (rem n %)))
                  (range 16632 (inc (/ n 2))))))

#_(first (filter #(>= (* 11 (apply + (divisors %)))
                      36000000)
               (range 831600 900900 5)))
