;;; format.scm --- Unit tests for `format'.

;; Copyright (C) 2005, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite format-tests
  "CL-style `format'")

(define-test-case format-tests a/strings ()
  (test-equal "a-fooish-bar-1"
    (format #f "a-~a-~a-~s" "fooish" "bar" 1)))

(define-test-case format-tests s/symbols ()
  (test-equal "a-fooish-bar-2"
    (format #f "a-~s-~s-~s" 'fooish 'bar 2)))

(define-test-case format-tests s/string ()
  (test-equal "a-\"fooish\"-bar-3"
    (format #f "a-~s-~s-~s" "fooish" 'bar 3)))

(run-test-suite format-tests)

;;; format.scm ends here
