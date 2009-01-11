;;; time-lib.scm --- 

;; Copyright (C) 2004, 2005-2006, 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(define *posix-epoch* (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))

(define/optional-args (posix-timestamp->time-utc timestamp (optional (nanoseconds 0)))
  (add-duration *posix-epoch* (make-time time-duration nanoseconds timestamp)))

(define (time-utc->posix-timestamp time-utc)
  (time-second (time-difference time-utc *posix-epoch*)))
