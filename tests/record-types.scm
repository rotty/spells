;;; record-types.scm --- unit tests for (spells record-types)

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (srfi :8 receive)
        (spells record-types)
        (wak trc-testing))

(define-test-suite record-types-tests
  "Record types")

(define-record-type* foo
  (make-foo a b)
  ((c 1)))

(define-functional-fields foo a b)

(define-test-case record-types-tests basics ()
  (let ((f (make-foo 3 2)))
    (test-equal '(3 2 1)
      (list (foo-a f) (foo-b f) (foo-c f)))))

(define-test-case record-types-tests setters ()
  (let ()
    (define-record-type* bar
      (make-bar (x) y)
      ((z 1)))
    (let ((b (make-bar 3 2)))
      (test-equal '(3 2 1)
        (list (bar-x b) (bar-y b) (bar-z b)))
      (set-bar-x! b 5)
      (test-eqv 5 (bar-x b)))))

(define-test-case record-types-tests functional-fields ()
  (let ((f (make-foo 3 2)))
    (test-equal '(3 2)
      (receive components (foo-components f)
        components))
    (test-equal '(3 4)
      (call-with-values
          (lambda () (foo-components (foo-with-b f 4)))
        list))))

(run-test-suite record-types-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-pn= 1))
;; End:
