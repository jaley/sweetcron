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

    t-next     - the time instant being proposed for next occurrence
    t-now      - the current time instant, for calculating next occurrence
    value      - numeric value of time unit
    set-fn     - fn to update dt with explicit new value of time unit
    carry-unit - on overflow, increment dt by 1 of this unit function"
  [t-next t-now value set-fn carry-unit]
  (let [t-proposed (-> t-next (set-fn value) (t/floor t/minute))]
    (cond-> t-next
      ;; if the proposed change puts us in the past, we need to carry
      (t/before? t-proposed t-now) (t/plus (-> 1 carry-unit))
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
  [t-next t-now month]
  (if (= month :*)
    t-next
    (-> t-next
        (roll-forward t-now month (memfn withMonthOfYear m) t/years)
        (zero-components :day :hour :minute))))

(defn advance-day-of-month
  "Update the day component according to a day-of-month constraint"
  [t-next t-now day-of-month]
  (if (= day-of-month :*)
    t-next
    (-> t-next
        (roll-forward t-now day-of-month (memfn withDayOfMonth d) t/months)
        (zero-components :hour :minute))))

(defn cron-dow->joda-dow
  "Mapping from cron (0-7) to Joda (1-7) day-of-week numbers"
  [cron-dow]
  (if (zero? cron-dow) 7 cron-dow))

(defn advance-day-of-week
  "Update the day component according to a day-of-week constraint"
  [t-next t-now day-of-week]
  (if (= day-of-week :*)
    t-next
    (let [joda-dow (cron-dow->joda-dow day-of-week)]
      (-> t-next
          (roll-forward t-now joda-dow (memfn withDayOfWeek d) t/weeks)
          (zero-components :hour :minute)))))

(defn advance-day
  "Update the day component to the next most recent of either the
  day-of-month or day-of-week options, if they are static."
  [t-next t-now day-of-month day-of-week]
  (if (= :* day-of-week day-of-month)
    t-next ;; no day constraints, runs every day
    (let [next-dom (advance-day-of-month t-next t-now day-of-month)
          next-dow (advance-day-of-week t-next t-now day-of-week)]
      (cond
        (= :* day-of-month) next-dow
        (= :* day-of-week)  next-dom
        ;; if both have constraints, we return the earliest of the two
        :else (min-key c/to-long next-dow next-dom)))))

(defn advance-hour
  "Update the hour component according to the given constraint"
  [t-next t-now hour]
  (if (= hour :*)
    t-next
    (-> t-next
        (roll-forward t-now hour (memfn withHourOfDay hour) t/days)
        (zero-components :minute))))

(defn advance-minute
  "Update the minute component according to the given constraint."
  [t-next t-now minute]
  (if (= :* minute)
    (t/plus t-next (-> 1 t/minutes)) ;; always return the next minute
    (roll-forward t-next t-now minute (memfn withMinuteOfHour minute) t/hours)))

(defrecord CronRecurrence [minute hour day-of-month month day-of-week]
  r/Recurrence
  (time-of-next [r t-now]
    (-> t-now
        (advance-month t-now month)
        (advance-day t-now day-of-month day-of-week)
        (advance-hour t-now hour)
        (advance-minute t-now minute)
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
