;;; algebraic-types.scm --- Testsuite for define-datatype

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite datatype-tests
  "Algebraic datatypes")

(define-test-case datatype-tests basics ()
  (let ()
    (define-datatype foo
      (bar (x y))
      (baz (z)))
    (test-equal '(hello world)
      (cases foo (make-bar 'hello 'world)
        ((bar a b) (list a b))))
    (test-equal 'else
      (cases foo (make-baz 42)
        ((bar a b) 'bar)
        (else      'else)))))

(run-test-suite datatype-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (cases 2))
;; End:
