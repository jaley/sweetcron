(ns sweetcron.recurrences.cron
  "Cron-like recurrence implementation"
  (:require [clojure.string :as str]
            [clj-time.core :as t]
            [sweetcron.recurrences.core :as r])
  (:import  [org.joda.time DateTime]))

(defn zero-components
  "Set the requested components to zero, after a roll-over"
  [^DateTime dt & components]
  (let [should-zero? (set components)]
    (cond-> dt
      (should-zero? :day)    (.withDayOfMonth 1)
      (should-zero? :hour)   (.withHourOfDay 0)
      (should-zero? :minute) (.withMinuteOfHour 0))))

(defn cron-dow->joda-dow
  "Mapping from cron (0-7) to Joda (1-7) day-of-week numbers"
  [cron-dow]
  (if (zero? cron-dow) 7 cron-dow))

(defn advance?
  "Predicate to check if a time component needs to be bumped"
  [cron-value t-value]
  (and (not= cron-value :*) (not= cron-value t-value)))

(defn advance-one-month
  "Move dt forward one month and zero smaller units"
  [dt]
  (-> dt
      (t/plus (t/months 1))
      (zero-components :day :hour :minute)))

(defn advance-one-day
  "Move dt forward one day and zero smaller units"
  [dt]
  (-> dt
      (t/plus (t/days 1))
      (zero-components :hour :minute)))

(defn advance-one-hour
  "Move dt forward one day and zero the minutes"
  [dt]
  (-> dt
      (t/plus (t/hours 1))
      (zero-components :minute)))

(defn advance-one-minute
  "Move dt forward one minute"
  [dt]
  (t/plus dt (t/minutes 1)))

(defn advance-to-next-occurrence
  "Move t-now forward until the next time it would trigger the cron
  tab components provided in other args."
  [t-now minute hour day-of-month month day-of-week]
  (let [joda-dow (if (= day-of-week :*) :* (cron-dow->joda-dow day-of-week))]
    (loop [t-next (t/plus t-now (t/minutes 1))]
      (cond
        (advance? month (t/month t-next))          (recur (advance-one-month t-next))
        (advance? day-of-month (t/day t-next))     (recur (advance-one-day t-next))
        (advance? joda-dow (t/day-of-week t-next)) (recur (advance-one-day t-next))
        (advance? hour (t/hour t-next))            (recur (advance-one-hour t-next))
        (advance? minute (t/minute t-next))        (recur (advance-one-minute t-next))
        :else                                      t-next))))

(defrecord CronRecurrence [minute hour day-of-month month day-of-week]
  r/Recurrence
  (time-of-next [r t-now]
    (-> t-now
        (t/floor t/minute)
        (advance-to-next-occurrence minute hour day-of-month
                                    month day-of-week))))

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
