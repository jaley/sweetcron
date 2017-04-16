(ns sweetcron.recurrences.core
  "Core recurrence abstraction, used for recurring job definitions")

(defprotocol Recurrence
  (time-of-next [r t-now]
    "Return a joda DateTime for the next instant this  recurrence next fires, 
     after t-now, also a DateTime instance."))
