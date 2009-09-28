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
  (export posix-timestamp->time-utc
          time-utc->posix-timestamp)
  (import
    (rnrs base)
    (srfi :19 time)
    (spells opt-args))

  (define *posix-epoch* (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))

  (define/optional-args (posix-timestamp->time-utc timestamp (optional (nanoseconds 0)))
    (add-duration *posix-epoch* (make-time time-duration nanoseconds timestamp)))

  (define (time-utc->posix-timestamp time-utc)
    (time-second (time-difference time-utc *posix-epoch*))))
