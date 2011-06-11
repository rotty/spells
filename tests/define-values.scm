;;; define-values.scm --- Testsuite for define-values

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (spells define-values)
        (wak trc-testing))

(define-test-suite define-values-tests
  "Algebraic define-valuess")

(define-test-case define-values-tests multi-value ()
  (test-equal '(1 2)
    (let ()
      (define-values (x y) (values 1 2))
      (list x y))))

(define-test-case define-values-tests single-value ()
  (test-eqv 2
    (let ()
      (define-values (x) 2)
      x)))

(define-test-case define-values-tests zero-values ()
  (test-eqv 42
    (let ((foo #f))
      (define-values ()
        (set! foo 42)
        ;; check that the continuation accepts any number of values
        (values 1 2 3))
      ;; this also tests that the above expands into a definition
      (define bar foo)
      foo)))

(define-test-case define-values-tests repeated ()
  (test-equal '(1 2 3)
    (let ()
      (define-values (foo bar) (values 1 2))
      (define-values (baz) 3)
      (list foo bar baz))))

(run-test-suite define-values-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (cases 2))
;; End:
