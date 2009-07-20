;;; operations.scm --- Unit tests for (spells operations)

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(define-test-suite ops-tests
  "Operations")

(define-test-case ops-tests application ()
  (test-equal '() ((object list)))
  (test-equal '(1 2 3) ((object list) 1 2 3)))

(define-test-case ops-tests operations ()
  (test-equal '(1 2 3) ((operation list) 1 2 3)))

(define-test-case ops-tests objs/ops ()
  (let* ((op (operation #f))
         (obj (object #f ((op a b c) (list a b c)))))
    (test-equal (list obj 1 2) (op obj 1 2))))

(define-test-case ops-tests join ()
  (let* ((op1 (operation #f))
         (op2 (operation #f))
         (o1 (object #f ((op1 self a b c) (list a b c))))
         (o2 (object #f ((op2 self a b c) (vector a b c))))
         (joined-obj (join o1 o2)))
    (test-equal (list 1 2 3) (op1 joined-obj 1 2 3))
    (test-equal (vector 1 2 3) (op2 joined-obj 1 2 3))))

(run-test-suite ops-tests)
