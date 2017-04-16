(ns sweetcron.recurrences.core
  "Core recurrence abstraction, used for recurring job definitions")

(defprotocol Recurrence
  (time-of-next [r t-now]
    "Return a joda DateTime for the instant this recurrence next fires,
     after t-now, also a DateTime instance. Should always return a DateTime
     in the future, *after* t-now. If the next occurrence is exactly at
     t-now, return the next one."))
