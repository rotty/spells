;;; zipper.scm --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite zipper-tests
  "Zipper data structure")

(define-test-case zipper-tests navigation ()
  (let ((z (make-zipper '(1 (foo) 3 (bar ("a" "b") baz)))))
    (test-eqv #t (zipper? z))
    (let* ((z1 (zip-down z))
           (z2 (zip-right z1))
           (z3 (zip-right z2))
           (z4 (zip-right z3)))
      (test-equal 1      (zipper-node z1))
      (test-equal '(foo) (zipper-node z2))
      (test-equal 3 (zipper-node z3))
      (test-equal '(bar ("a" "b") baz)
        (zipper-node z4))
      (test-equal (zipper-node z1)
        (zipper-node (zip-left z2)))
      (test-equal (zipper-node z2)
        (zipper-node (zip-left (zip-left z4))))
      (test-equal (zipper-node z)
        (zipper-node (zip-up z3))))))

(define-test-case zipper-tests manipulation ()
  (let ((z (make-zipper '(1 (foo) 3))))
    (test-equal '(1 (foo) bar)
      (zip-finish (zip-change (zip-right (zip-right (zip-down z)))
                              'bar)))
    (test-equal '(1 3)
      (zip-finish (zip-delete (zip-right (zip-down z)))))
    (test-equal '(1 (foo) 2 3)
      (zip-finish (zip-insert-right (zip-right (zip-down z)) 2)))
    (test-equal  '(1 2 (foo) 3)
      (zip-finish (zip-insert-left (zip-right (zip-down z)) 2)))
    (test-equal '(1 (frob foo) 3)
      (zip-finish (zip-insert-down (zip-right (zip-down z)) 'frob)))))

(run-test-suite zipper-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
