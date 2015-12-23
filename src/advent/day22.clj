(ns advent.day22
  (:require
   [clojure.pprint :as p]
   [clojure.set :refer [union intersection difference]]))

(def spells
  #{[53  4 0 0   0 0]
  [73  2 2 0   0 0]
  [113 0 0 7   0 6]
  [173 3 0 0   0 6]
  [229 0 0 0 101 5]}
  )

(def spell-names
  {[53  4 0 0   0 0] :missle
   [73  2 2 0   0 0] :drain
   [113 0 0 7   0 6] :shield
   [173 3 0 0   0 6] :poison
   [229 0 0 0 101 5] :recharge})

(def boss
  [0 8 0 0 0 0 0])

(defn update-effects [{:keys [effects] :as state} action]
  (let [effects (filter (comp pos? last)
                        (map #(update-in % [5] dec) effects))]
    (let [next-mana (- (:mana state) (first action))]
      (if (and
           (pos? (last action))
           #_(pos? next-mana)
           #_(not ((set (map first effects)) (first action))))
        (assoc state
               :cost (+ (:cost state) (first action))
               :mana next-mana
               :effects (conj effects action))
        (assoc state :effects effects)))))

(defn apply-effects [state action]
  (let [;{:keys [effects] :as state} (update-effects state action)
        effects (reduce #(map + %1 %2) [0 0 0 0 0 0] (:effects state))
        state   (-> state
                    (update-in [:boss]   - (second effects))
                    (update-in [:mana]   + (nth effects 4))
                    #_(update-in [:player] + (nth effects 3)))]
    (update-effects
     (if (and (= boss action)
              (pos? (:boss state)))
       (update-in state [:player] - (max 1 (- (second action) (nth effects 3))))
       (if (and (zero? (last action)) (not= boss action))
         ;; immediate
         (assoc state
                :cost   (+ (:cost state)   (first action)) 
                :mana   (- (:mana state)   (first action))
                :boss   (- (:boss state)   (second action))
                :player (+ (:player state) (nth action 2)))
         state))
     action)))

(def tstate
  {:player 50
   :boss 55
   :mana 500
   :cost 0})

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

(defn game [state cost-so-far depth player?]
  (let [winner? (win? state)]
    (if (or (zero? depth) winner?)
      (if (and winner? (not= winner? :player))
        30000000                ;; ended up tweaking this value a bit
        (max 0 (+ cost-so-far (* 10 (- (:boss state) (:player state))))))
    (if player?
      (reduce min 30000000
              (map #(game (apply-effects state %) (+ 100000 (first %) cost-so-far) (dec depth) (not player?))
                   (possible-moves state)))
      (game (apply-effects state boss)
            (+ 10 cost-so-far)
            (dec depth)  
            (not player?))))))

(defn which-move [state search-depth]
  (let [mvs (sort-by second
                     (map #(vector
                            %
                            (game (apply-effects state %) (first %) search-depth false)) (possible-moves state)))
        mvs' (group-by second mvs)
        k   (apply min (keys mvs'))]
    (prn mvs')
    ;; select a random minimum
    #_(ffirst mvs) 
    (first (rand-nth (get mvs' k)))))

(defn step-state [depth state]
  (let [move  (which-move state depth)
        state (apply-effects state move)
        next-state (if (win? state)
                     state
                     (apply-effects state boss))]
    (prn move)
    (prn (spell-names move))
    (prn (count (possible-moves next-state)))
    (prn next-state)
    (prn (:cost next-state))
    next-state))

; part 1
#_(first (filter win? (iterate (partial step-state 17)
                               tstate)))

(defn hard-game [game-fn state action]
  (if (not= boss action)
    (let [nstate (update-in state [:player] dec)]
      (if (not (pos? (:player nstate)))
        nstate ;; game over 
        (game-fn nstate action)))
    (game-fn state action)))

; part 2
#_(with-redefs [apply-effects (partial hard-game apply-effects)]
    (first (filter win? (iterate (partial step-state 22)
                               tstate))))
