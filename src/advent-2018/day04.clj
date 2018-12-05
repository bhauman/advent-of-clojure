(ns advent-2018.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as med]
   [clj-time.core :as t]))

(defn extract-log-entry [[[type] [year month day hour min gid]]]
  (cond-> {:date [(+ year 500) month day]
           :time [hour min]
           :type (keyword (string/lower-case type))}
    gid (assoc :gid gid)))

(defn advance-guard-date [{:keys [date gid time] :as log-entry}]
  (cond-> log-entry
    (and gid (= 23 (first time)))
    (update :date
            #((juxt t/year t/month t/day)
              (t/plus (apply t/date-time %) (t/days 1))))))

(defn time-intervals [logs]
  (let [res (group-by :gid logs)]
    [(some identity (keys res))
     (->> (get res nil)
          (map (comp second :time))
          sort
          (partition 2))]))

(def guard-sleep-intervals
  (->> (line-seq (io/reader (io/resource "2018/day04")))
       (map (juxt
             (some-fn (partial re-seq #"Guard")
                      (partial re-seq #"wakes")
                      (partial re-seq #"falls"))
             #(map (fn [x] (Integer/parseInt x))
                   (re-seq #"\d+" %))))
       (map extract-log-entry)
       (map advance-guard-date)
       (group-by :date)
       vals
       (map time-intervals)))

(defn time-asleep [intervals]
  (->> intervals
       (map #(apply - (reverse %)))
       (apply +)))

(defn slept-the-most [interval-report]
  (->> interval-report
       (map #(update-in % [1] time-asleep))
       (group-by first)
       (med/map-vals #(apply + (map second %)))
       (sort-by val >)
       ffirst))

#_(slept-the-most guard-sleep-intervals)

(defn best-minute-for-guard [gid interval-report]
  (->> interval-report
       (filter #(= gid (first %)))
       (mapcat second)
       (mapcat #(apply range %))
       frequencies
       (sort-by second >)
       first))

#_(best-minute-for-guard 421 guard-sleep-intervals)

;; part 1
#_ (= (let [gid (slept-the-most guard-sleep-intervals)]
        (* (first (best-minute-for-guard gid guard-sleep-intervals))
           gid))
      11367)

;; part 2
#_(let [guard-ids (distinct (map first guard-sleep-intervals))]
    (->> (keep #(when-let [res (best-minute-for-guard % guard-sleep-intervals)]
                  {:guard % :minute (first res) :total-time (second res)})
               guard-ids)
         (sort-by :total-time >)
         first
         ((juxt :guard :minute))
         (apply *)))






