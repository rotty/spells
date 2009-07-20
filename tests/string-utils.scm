;;; string-utils.scm --- Unit tests for (spells string-utils)

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(define-test-suite strutils-tests
  "String utilities")

(define-test-suite (strutils-tests.split strutils-tests)
  "string-split")

(define-test-case strutils-tests.split charset ()
  (test-equal '("" "abc" "d" "e" "" "f" "") (string-split ":abc:d:e::f:" '(#\:)))
  (test-equal '("" "") (string-split ":" '(#\:)))
  (test-equal '("root" "x:0:0:Lord") (string-split "root:x:0:0:Lord" '(#\:) 2))
  (test-equal '("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
    (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:)))
  (test-equal '("" "usr" "local" "bin") (string-split "/usr/local/bin" '(#\/))))

(define-test-suite (strutils-tests.subst strutils-tests)
  "string-substitute")

(define-test-case strutils-tests.subst vectors ()
  (test-equal "Welcome, Bob."
    (string-substitute "Welcome, {0}." '#("Bob")))
  (test-equal "Today is the Nov 13, 2008"
    (string-substitute "Today is the {1} {2}, {0}" '#("2008" "Nov" "13"))))

(run-test-suite strutils-tests)
