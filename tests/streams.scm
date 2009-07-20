;; streams.scm --- unit tests for spells.streams

;; Copyright (C) 2006, 2009 by Andreas Rottmann

;; Author: Andreas Rottmann
;; Start date: Sat May 20 20:01:51 CEST 2006

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(let ((s (stream 1 2 3)))
  (testeez "basic"
    (test/equal "car" (stream-car s) 1)))

