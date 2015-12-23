(ns advent.day22
  (:require
   [clojure.pprint :as p]
   [clojure.set :refer [union intersection difference]]))

(def spells
  #{[53  4 0 0   0 0]
  [73  2 2 0   0 0]
  [113 0 0 7   0 6]
  [173 3 0 0   0 6]
  [229 0 0 0 101 5]})

(def boss
  [0 8 0 0 0 0 0])

(defn update-effects [{:keys [effects] :as state} action]
  (let [effects (filter (comp pos? last)
                        (map #(update-in % [5] dec) effects))]
    (if (pos? (last action))
      (-> state
          (update-in [:cost] + (first action))
          (update-in [:mana] - (first action))
          (assoc :effects (conj effects action)))
      (assoc state :effects effects))))

(defn apply-effects [state action]
  (let [effects (reduce #(map + %1 %2) [0 0 0 0 0 0] (:effects state))
        state   (-> state
                    (update-in [:boss]   - (second effects))
                    (update-in [:mana]   + (nth effects 4)))]
    (update-effects
     (if (and (= boss action)
              (pos? (:boss state)))
       (update-in state [:player] - (max 1 (- (second action) (nth effects 3))))
       (if (and (zero? (last action)) (not= boss action))
         ;; immediate
         (-> state
             (update-in [:cost] + (first action))
             (update-in [:mana] - (first action))
             (update-in [:boss] - (second action))
             (update-in [:player] + (nth action 2)))
         state))
     action)))

(def tstate {:player 50 :boss 55 :mana 500 :cost 0})

(defn possible-moves [state]
  (let [ids (difference 
             (set (map first spells))
             (set (map first (filter #(< 1 (last %)) (:effects state)))))]
    (filter
     #(>= (:mana state) (first %))
     (filter #(ids (first %)) spells))))

(defn win? [{:keys [player boss mana] :as state}]
  (or (and (or (not (pos? boss))
               (not (pos? player)))
           (if (> player boss) :player :boss))
      (and (zero? (count (possible-moves state))) :boss)
      (and (<= mana 0) :boss)))

;; this is the magic sauce
;; evaluate the rate of damage and make sure we don't get too far ahead
(defn cost-fn [state]
  (let [spent (:cost state)
        total-damage (- (:boss tstate) (:boss state))
        total-loss   (- (:player tstate) (:player state))
        distance     (java.lang.Math/abs (- (:player state) (:boss state)))]
    (float (- (- (/ total-damage spent)
                 (/ distance (/ spent 1.0005))
                 (/ total-loss spent))))))

#_(< (cost-fn {:player 16, :boss 12, :mana 141, :cost 1368, :effects ()})
     (cost-fn {:player 12, :boss 12, :mana 141, :cost 1369, :effects ()}))

(defn game [state depth player?]
  (let [winner? (win? state)]
    (if (or (zero? depth) winner?)
      (if (and winner? (not= winner? :player))
        30000000
        (cost-fn state))
      (if player?
        (reduce min 30000000
                (map #(game (apply-effects state %) (dec depth) (not player?))
                     (possible-moves state)))
        (game (apply-effects state boss) (dec depth) (not player?))))))

(defn which-move [state search-depth]
  (let [mvs (sort-by second
                     (map #(vector
                            %
                            (game (apply-effects state %) search-depth false)) (possible-moves state)))
        mvs' (group-by second mvs)
        k   (apply min (keys mvs'))]
    #_(prn mvs')
    (first (rand-nth (get mvs' k)))))

(defn step-state [depth state]
  (let [move  (which-move state depth)
        state (apply-effects state move)
        next-state (if (win? state)
                     state
                     (apply-effects state boss))]
    (prn move)
    #_(prn (spell-names move))
    #_(prn (count (possible-moves next-state)))
    (prn next-state)
    (prn (:cost next-state))
    next-state))

;; part 1
#_(with-redefs [game (memoize game)]
    (first (filter win? (iterate (partial step-state 22) tstate))))

(defn hard-game [game-fn state action]
  (if (not= boss action)
    (let [nstate (update-in state [:player] dec)]
      (if (not (pos? (:player nstate)))
        nstate ;; game over 
        (game-fn nstate action)))
    (game-fn state action)))

; part 2
#_(with-redefs [game (memoize game)
                apply-effects (partial hard-game apply-effects)]
    (first (filter win? (iterate (partial step-state 23)
                               tstate))))




