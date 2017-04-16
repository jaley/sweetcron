(ns sweetcron.recurrences.cron
  "Cron-like recurrence implementation"
  (:require [clojure.string :as str]
            [clj-time
             [core   :as t]
             [coerce :as c]]
            [sweetcron.recurrences.core :as r])
  (:import  [org.joda.time DateTime]))

(defn roll-forward
  "Rolls a date foward according to time unit rules specified in other
  params:

    value      - numeric value of time unit
    get-fn     - fn to return current value of time unit from dt
    set-fn     - fn to update dt with explicit new value of time unit
    carry-unit - on overflow, increment dt by 1 of this unit function"
  [dt value get-fn set-fn carry-unit]
  (let [proposed (-> dt (set-fn value) (t/floor t/minute))]
    (cond-> dt
      (not (t/after? proposed dt)) (t/plus (-> 1 carry-unit))
      true                         (set-fn value))))

(defn zero-components
  "Set the requested components to zero, after a roll-over"
  [^DateTime dt & components]
  (let [should-zero? (set components)]
    (cond-> dt
      (should-zero? :day)    (.withDayOfMonth 1)
      (should-zero? :hour)   (.withHourOfDay 0)
      (should-zero? :minute) (.withMinuteOfHour 0))))

(defn advance-month
  "Update the month component of given date time, if the month
  parameter is a static value."
  [dt month]
  (if (= month :*)
    dt
    (-> dt
        (roll-forward month t/month (memfn withMonthOfYear m) t/years)
        (zero-components :day :hour :minute))))

(defn advance-day-of-month
  "Update the day component according to a day-of-month constraint"
  [dt day-of-month]
  (if (= day-of-month :*)
    dt
    (-> dt
        (roll-forward day-of-month t/day (memfn withDayOfMonth d) t/months)
        (zero-components :hour :minute))))

(defn cron-dow->joda-dow
  "Mapping from cron (0-7) to Joda (1-7) day-of-week numbers"
  [cron-dow]
  (if (zero? cron-dow) 7 cron-dow))

(defn advance-day-of-week
  "Update the day component according to a day-of-week constraint"
  [dt day-of-week]
  (if (= day-of-week :*)
    dt
    (let [joda-dow (cron-dow->joda-dow day-of-week)]
      (-> dt
          (roll-forward joda-dow t/day-of-week (memfn withDayOfWeek d) t/weeks)
          (zero-components :hour :minute)))))

(defn advance-day
  "Update the day component to the next most recent of either the
  day-of-month or day-of-week options, if they are static."
  [dt day-of-month day-of-week]
  (if (= :* day-of-week day-of-month)
    dt ;; no day constraints, runs every day
    (let [next-dom (advance-day-of-month dt day-of-month)
          next-dow (advance-day-of-week dt day-of-week)]
      (cond
        (= :* day-of-month) next-dow
        (= :* day-of-week)  next-dom
        ;; if both have constraints, we return the earliest of the two
        :else (min-key c/to-long next-dow next-dom)))))

(defn advance-hour
  "Update the hour component according to the given constraint"
  [dt hour]
  (if (= hour :*)
    dt
    (-> dt
        (roll-forward hour t/hour (memfn withHourOfDay hour) t/days)
        (zero-components :minute))))

(defn advance-minute
  "Update the minute component according to the given constraint."
  [dt minute]
  (if (= :* minute)
    (t/plus dt (-> 1 t/minutes)) ;; always return the next minute
    (roll-forward dt minute t/minute (memfn withMinuteOfHour minute) t/hours)))

(defrecord CronRecurrence [minute hour day-of-month month day-of-week]
  r/Recurrence
  (time-of-next [r t-now]
    (-> t-now
        (advance-month month)
        (advance-day day-of-month day-of-week)
        (advance-hour hour)
        (advance-minute minute)
        (t/floor t/minute))))

(defn numeric?
  "Tests if the given string has numeric only content"
  [s]
  (re-seq #"[0-9]+" s))

(defn parse-cron-tab
  "Return a Cron-like recurrence implementation for a given crontab string.
  Supports only basic * and numerical values currently."
  [cron-tab]
  (->> (str/split cron-tab #"\s")
       (take 5)
       (map (fn [c]
              (cond
                (= "*" c)    :*
                (numeric? c) (Long/parseUnsignedLong c)
                :else        (throw
                              (ex-info "Invalid cron-tab component"
                                       {:cron-tab  cron-tab
                                        :component c})))))
       (apply ->CronRecurrence)))
