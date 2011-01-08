;;; list-utils.scm --- Unit tests for (spells list-utils)

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (wak trc-testing)
        (spells list-utils))

(define-test-suite list-utils-tests
  "List utilities")

(define-test-case list-utils-tests list-intersperse ()
  (test-equal '() (list-intersperse '() 'x))
  (test-equal '(1) (list-intersperse '(1) 'x))
  (test-equal '(1 x 2 x 3) (list-intersperse '(1 2 3) 'x)))

(run-test-suite list-utils-tests)
