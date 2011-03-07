;;; finite-types.scm --- unit tests for (spells finite-types)

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (srfi :8 receive)
        (spells finite-types)
        (wak trc-testing))

(define-test-suite finite-types-tests
  "Finite types")

(define-finite-type foo <foo>
  (a b)
  foo?
  foo-vector
  foo/name
  foo/index

  (a foo/a)
  (b foo/b)

  ((one 1 "One")
   (two 2 "Two")))

(define-enumerated-type bar <bar>
  bar?
  bar-vector
  bar/name
  bar/index
  (one two))

(define-test-case finite-types-tests basics ()
  (let ((f1 (foo one)))
    (test-eqv #t (foo? f1))
    (test-eqv #f (bar? f1))
    (test-eq 'one (foo/name f1))
    (test-eqv 0 (foo/index f1))
    (test-eq f1 (vector-ref foo-vector (foo/index f1)))
    (test-equal '(1 "One")
      (list (foo/a f1) (foo/b f1))))
  (let ((f2 (foo two)))
    (test-eqv 1 (foo/index f2))
    (test-equal '(2 "Two")
      (list (foo/a f2) (foo/b f2)))))

(define-test-case finite-types-tests enumerated ()
  (let ((b1 (bar one)))
    (test-eqv #t (bar? b1))
    (test-eqv #f (foo? b1))
    (test-eq 'one (bar/name b1))
    (test-eqv 0 (bar/index b1))
    (test-eq b1 (vector-ref bar-vector (bar/index b1)))))

(run-test-suite finite-types-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-pn= 1))
;; End:
