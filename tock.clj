(ns tock
  "Extensions and helpers for `tick`"
  (:require [tick.core      :as tick]))

(defn
  start-of-day
  "Given a 'time-str'
  Return the `tick/inst` of the start of that day"
  [time-str]
  (-> time-str
      (str "T00:00")
      tick/date-time
      tick/instant))

(defn
  seconds-to-days
  [seconds]
  (* seconds
     (/ 1.0
        (* 24
           60
           60))))

(defn
  unix-time-sec
  "The unix time of the start of a certain date string
  ex: 2023-01-01"
  [date]
  (->> date
       tick/instant
       (tick/between (tick/epoch))
       tick/seconds
       abs))
#_
(-> #inst "2023-01-01"
    unix-time)
;; => 1672531200

(defn
  unix-time-sec2date
  "Convert a `unix-time-sec` in to a date
  at the given timezone
  Defaults to `Asia/Bangkok`
  Full this available here:
  https://code2care.org/pages/java-timezone-list-utc-gmt-offset/
  with many options"
  [unix-timestamp
   & [{:keys [timezone]
       :or   {timezone "Asia/Bangkok"}}]]
  (-> unix-timestamp
      long
      (* 1000)
      tick/instant
      (cljc.java-time.zoned-date-time/of-instant (tick/zone timezone))
      tick/date-time #_
      str))
#_
(-> 1672531200
    unix-time-sec2date)
#_
(->> #inst "2023-01-01"
     unix-time-sec
     unix-time-sec2date
     (tick/format (tick/formatter "dMMM\nyy")))
;; => #time/date-time "2023-01-01T00:00"

(defn
  date2date-str
  "Take a `date` and format it to a string that can be used in a plot
  Formatter is documented here:
  https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
  it has many many options"
  [date]
  (->> date
       tick/date
       (tick/format (tick/formatter "ddMMM''yy"))))
#_
(date2date-str #inst"2022-11-27")

(defn
  unix-time-sec2date-str
  "Take a `unix-time` integer
  and convert it to a formatted string of the date
  Note:
  Primary use here is for plotting axis"
  [unix-time-sec]
  (->> unix-time-sec
       unix-time-sec2date
       date2date-str))

(-> 1672531200
    unix-time-sec2date-str)

(defn
  months-in-range
  "Get a list of dates for the start of all months
  between `start-time` and `end-time`"
  [start-time
   end-time]
  (let [next-month (-> start-time
                       tick/date
                       (tick/>> (tick/new-period 1 :months))
                       tick/first-day-of-month
                       tick/date)]
    (if (tick/> next-month
                (-> end-time
                    tick/date))
      [] ;; base-case, return empty list
      ;; else append month's unix-time - no-recur so could blow stack
      (conj (months-in-range next-month
                             end-time)
            next-month))))
#_
(tick/>> (tick/in #inst"2023-11-27"
                  "Asia/Bangkok")
         (tick/new-period 1 :months))

(defn
  month-start-unix-time
  "Give an `start-date` and `end-date`
  Finds all the start/end points of months
  ex: 2023-07-01 00:00
  And finds the associate `unix-time` for it
  Note:
  You can specify a time-zone.
  Defaults to `Asia/Bangkok`"
  [date
   & [{:keys [time-zone]
       :or   {time-zone "Asia/Bangkok"}}]
   (-> date                     ;; #inst "2023-10-27"
       tick/date                ;; #time/date "2023-10-27"
       tick/first-day-of-month  ;; #time/date "2023-10-01"
       tick/midnight            ;; #time/date-time "2023-10-01T00:00"
       (tick/in time-zone) ;; #time/zoned-date-time "2023-10-01T00:00+07:00[Asia/Bangkok]";; =>
       tock/unix-time-sec     ;; 1696093200
       #_
       tock/unix-time-sec2date))
  ;; ex: #time/date-time "2023-10-31T17:00"
  #_
  (-> #inst "2023-10-27"
      month-start-unix-time
      tock/unix-time-sec2date
      (tick/in "UTC")
      (tick/in "Asia/Bangkok"))
  ;; => #time/zoned-date-time "2023-10-01T00:00+07:00[Asia/Bangkok]"

  (defn
    month-start-unix-times
    "Give an `start-date` and `end-date`
  Finds all the start/end points of months
  ex: 2023-07-01 00:00
  And finds the associate `unix-time` for it
  Defaults to `Asia/Thailand`"
    [start-date
     end-date
     & [{:keys [time-zone]
         :or   {time-zone "Asia/Bangkok"}}]]
    (->> (months-in-range (tick/date start-date) ; in case `date-time` given
                          (tick/date end-date)) ;   will convert to a `date`
         (mapv month-start-unix-time)))

  (month-start-unix-times #inst"2022-11-27"
                          #inst"2023-11-27"
                          {:time-zone time-zone})
  ;; => [1698771600
  ;;     1696093200
  ;;     1693501200
  ;;     1690822800
  ;;     1688144400
  ;;     1685552400
  ;;     1682874000
  ;;     1680282000
  ;;     1677603600
  ;;     1675184400
  ;;     1672506000
  ;;     1669827600]
