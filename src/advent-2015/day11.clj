(ns advent-2015.day11)

(defn straight-test [x]
  (some (fn [r]
          (let [[a b c] (map byte r)]
            (and a b c (= (+ 2 a) (inc b) c))))
        (partition 3 1 x)))

(defn not-iol [x]
  (not (re-matches #".*(i|o|l).*" (apply str x)))) 

(defn has-different-pairs [x]
  (< 1
     (count (set (filter #(apply = %) (partition 2 1 x))))))

(defn valid-password? [p]
  (and
   (not-iol p)
   (has-different-pairs p)
   (straight-test p)))

(defn adjust-char [x]
  (char
   (+
    (if (<= (byte x) (byte \9)) 49 10)
    (byte x))))

(defn adjust-char-back [x]
  (char
   (-
    (byte x)
    (if (<= (byte x) (byte \j)) 49 10))))

(def base26->password #(apply str (mapv adjust-char %)))

(def password->base26 #(apply str (mapv adjust-char-back %)))

(defn long->password [i]
  (-> i
      (java.lang.Long/toString 26)
      base26->password))

(defn password->long [s]
  (-> s
      password->base26
      (java.lang.Long/valueOf 26)))

(defn base26-passwords [start-int]
  (filter valid-password?
          (map long->password (iterate inc start-int))))

;; part 1
#_(first (base26-passwords (password->long "cqjxjnds")))

;; part 2
#_(second (base26-passwords (password->long "cqjxxyzz")))
