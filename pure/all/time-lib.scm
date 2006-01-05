;; -*- Mode: Scheme; scheme48-package: spells.time-lib; -*-

;; time-lib.scm - Implementation-independent part of spells.time-lib

;; Copyright (C) 2004, 2005-2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Wed Jan  4 13:33:55 CET 2006

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

(define *posix-epoch* (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))

(define (posix-timestamp->time-utc timestamp)
  (add-duration *posix-epoch* (make-time time-duration 0 timestamp)))

(define (time-utc->posix-timestamp time-utc)
  (time-difference time-utc *posix-epoch*))
