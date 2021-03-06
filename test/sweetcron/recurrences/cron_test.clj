(ns sweetcron.recurrences.cron-test
  "Unit tests for cron recurrences"
  (:require [sweetcron.recurrences
             [cron :as sut]
             [core :as rec]]
            [expectations :as e :refer [expect]]
            [clj-time
             [core   :as t]
             [format :as fmt]]))

(defn inst
  "Convenience for comparing to expected joda time instances.
  Uses UTC to prevent tests failing when time zones change."
  [tf]
  (let [formatter (fmt/formatter "yyyy-MM-dd HH:mm:ss" t/utc)]
    (fmt/parse formatter tf)))

(defn t-next
  "Construct a cron recurrence from cron-tab, then calculate next
  occurrence from given starting time."
  [start cron]
  (-> cron
      (sut/parse-cron-tab)
      (rec/time-of-next start)))

;; every minute
(expect
 (inst "2017-01-01 00:01:00")
 (t-next (inst "2017-01-01 00:00:00") "* * * * *"))

(expect
 (inst "2017-01-01 00:01:00")
 (t-next (inst "2017-01-01 00:00:59") "* * * * *"))

;; every hour
(expect
 (inst "2017-01-01 00:10:00")
 (t-next (inst "2017-01-01 00:00:00") "10 * * * *"))

(expect
 (inst "2017-01-01 01:10:00")
 (t-next (inst "2017-01-01 00:11:00") "10 * * * *"))

;; every day
(expect
 (inst "2017-01-01 07:00:00")
 (t-next (inst "2017-01-01 06:23:09") "0 7 * * *"))

(expect
 (inst "2017-01-02 07:00:00")
 (t-next (inst "2017-01-01 07:00:01") "0 7 * * *"))

;; every week
(expect
 ;; 16th April 2017 is a Sunday,
 (inst "2017-04-16 00:10:00")
 ;; note cron-tab sytnax for days is 0-7, where 0 and 7 are Sunday
 (t-next (inst "2017-04-14 15:22:15") "10 0 * * 0"))

(expect
 (inst "2017-04-16 00:10:00")
 (t-next (inst "2017-04-16 00:01:00") "10 0 * * 0"))

(expect
 (inst "2017-04-23 00:10:00")
 (t-next (inst "2017-04-16 00:11:00") "10 0 * * 0"))

;; every month
(expect
 (inst "2017-04-17 03:15:00")
 (t-next (inst "2017-04-16 17:00:00") "15 3 17 * *"))

(expect
 (inst "2017-05-17 03:15:00")
 (t-next (inst "2017-04-17 04:00:00") "15 3 17 * *"))

;; TODO: This test will currently fail, as the algorithm doesn't exactly
;; match cron behaviour for cases where both day-of-month and day-of-week
;; are set. See issue #1
;; (expect
;;  (inst "2017-04-23 03:15:00")
;;  (t-next (inst "2017-04-17 04:00:00") "15 3 17 * 0"))

;; annually
(expect
 (inst "2017-02-01 10:30:00")
 (t-next (inst "2017-01-16 14:00:00") "30 10 1 2 *"))

(expect
 (inst "2018-02-01 10:30:00")
 (t-next (inst "2017-02-02 14:00:00") "30 10 1 2 *"))
