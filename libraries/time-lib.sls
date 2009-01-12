;;; time-lib.sls --- Time library.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells time-lib)
  (export
    time make-time time? time-type time-nanosecond time-second
    date make-date date? date-nanosecond date-second date-minute 
    date-hour date-day date-month date-year date-zone-offset
    time-tai time-utc time-monotonic
    #|time-thread time-process|# time-duration
    read-leap-second-table copy-time current-time
    time-resolution time=? time>? time<? time>=? time<=?
    time-difference time-difference! add-duration
    add-duration! subtract-duration subtract-duration!
    time-tai->time-utc time-tai->time-utc! time-utc->time-tai
    time-utc->time-tai! time-monotonic->time-utc
    time-monotonic->time-utc! time-monotonic->time-tai
    time-monotonic->time-tai! time-utc->time-monotonic
    time-utc->time-monotonic! time-tai->time-monotonic
    time-tai->time-monotonic! time-tai->date time-utc->date
    time-monotonic->date date->time-utc date->time-tai
    date->time-monotonic leap-year? date-year-day
    date-week-day date-week-number current-date
    date->julian-day date->modified-julian-day
    time-utc->julian-day time-utc->modified-julian-day
    time-tai->julian-day time-tai->modified-julian-day
    time-monotonic->julian-day
    time-monotonic->modified-julian-day julian-day->time-utc
    julian-day->time-tai julian-day->time-monotonic
    julian-day->date modified-julian-day->date
    modified-julian-day->time-utc
    modified-julian-day->time-tai
    modified-julian-day->time-monotonic current-julian-day
    current-modified-julian-day date->string string->date

    ;; spells extensions
    posix-timestamp->time-utc
    time-utc->posix-timestamp
    )
  (import
    (rnrs base)
    (xitomatl srfi time)
    (spells opt-args))

  (define *posix-epoch* (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))

  (define/optional-args (posix-timestamp->time-utc timestamp (optional (nanoseconds 0)))
    (add-duration *posix-epoch* (make-time time-duration nanoseconds timestamp)))

  (define (time-utc->posix-timestamp time-utc)
    (time-second (time-difference time-utc *posix-epoch*))))
