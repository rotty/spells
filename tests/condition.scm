;; condition.scm -- Tests for spells's condition system
;; arch-tag: b8f9b41b-34d0-47ee-81a2-6c7612f65aef

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Sun May 22, 2005 16:36

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; Guile's module system sucks wrt. macros
(all-dialects-except guile)

(testeez
 "srfi-34"
 (test/equal
  "raise"
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (x) (k x))
                             (lambda ()
                               (+ 1 (raise "ex"))))))
  "ex")
 (test-define "x" x 0)
 (test/eq "guard0" (guard (condition
                           (else
                            (set! x (+ x 1))
                            'dont-care))
                          (+ 1 (raise 'an-error)))
          'dont-care)
 (test/eqv "x" x 1)
 (test/eq
  "guard1"
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (x)
                               (display "reraised ") (write x) (newline)
                               (k 'zero))
                             (lambda ()
                               (guard (condition
                                       ((positive? condition) 'positive)
                                       ((negative? condition) 'negative))
                                      (raise 1))))))
  'positive)
 (test/eq
  "guard2"
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler (lambda (x)
                               (display "reraised ") (write x) (newline)
                               (k 'zero))
                             (lambda ()
                               (guard (condition
                                       ((positive? condition) 'positive)
                                       ((negative? condition) 'negative))
                                      (raise -1))))))
  'negative)
 (test/eq  "guard3" (guard (condition
                            ((assq 'a condition) => cdr)
                            ((assq 'b condition)))
                           (raise (list (cons 'a 42))))
           42)
 (test/equal "guard4" (guard (condition
                              ((assq 'a condition) => cdr)
                              ((assq 'b condition)))
                             (raise (list (cons 'b 23))))
             '(b . 23)))

(define-condition-type &c &condition
  c?
  (x c-x))

(define-condition-type &c1 &c
  c1?
  (a c1-a))

(define-condition-type &c2 &c
  c2?
  (b c2-b))

(let* ((v1 (make-condition &c1 'x "V1" 'a "a1"))
       (v2 (condition (&c2 (x "V2") (b "b2"))))
       (v3 (condition (&c1 (x "V3/1") (a "a3")) (&c2 (b "b3"))))
       (v4 (make-compound-condition v1 v2))
       (v5 (make-compound-condition v2 v3)))


  (testeez
   "srfi-35"
   (test-true "base type predicate" (c? v1))
   (test-true "derived type predicate" (c1? v1))
   (test-true "other type predicate" (not (c2? v1)))
   (test-true "field accessor 1" (string=? (c-x v1) "V1"))
   (test-true "field accessor 2" (string=? (c1-a v1) "a1"))

   (test-true "base type predicate" (c? v2))
   (test-true "derived type predicate " (c2? v2))
   (test-true "other type predicate" (not (c1? v2)))
   (test-true "field accessor 1" (string=? (c-x v2) "V2"))
   (test-true "field accessor 2" (string=? (c2-b v2) "b2"))

   (test-true "base type predicate" (c? v3))
   (test-true "derived type predicate" (c1? v3))
   (test-true "other type predicate" (c2? v3))
   (test-true "field accessor 1" (string=? (c-x v3) "V3/1"))
   (test-true "field accessor 2" (string=? (c1-a v3) "a3"))
   (test-true "field accessor 3" (string=? (c2-b v3) "b3"))

   (test-true "base type predicate" (c? v4))
   (test-true "derived type predicate 1" (c1? v4))
   (test-true "derived type predicate 2" (c2? v4))
   (test-true "field accessor 1" (string=? (c-x v4) "V1"))
   (test-true "field accessor 2" (string=? (c1-a v4) "a1"))
   (test-true "field accessor 3" (string=? (c2-b v4) "b2"))

   (test-true "base type predicate" (c? v5))
   (test-true "derived type predicate 1" (c1? v5))
   (test-true "derived type predicate 2" (c2? v5))
   (test-true "field accessor 1" (string=? (c-x v5) "V2"))
   (test-true "field accessor 2" (string=? (c1-a v5) "a3"))
   (test-true "field accessor 3" (string=? (c2-b v5) "b2"))))


;;; condition.scm ends here
