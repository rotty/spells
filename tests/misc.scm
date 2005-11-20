;; misc.scm -- Unit tests for spells.util.misc
;; arch-tag: 4822bd68-4e16-4bbd-bca4-42277a933717

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Jun 13, 2005 07:13

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

;;; Code:

(let ((thunk (lambda () #t))
      (proc (lambda (x) (+ x 1)))
      (proc1 (lambda (x y z w) (display "hello") ((x y z) w)))
      (proc2 (lambda x (list x)))
      (proc3 (lambda (x . y) (cons x y)))
      (proc4 (lambda (x w . y) #f)))
  (testeez
   "thunk?"
   (test-true "0" (thunk? thunk))
   (test-true "x" (thunk? proc2))
   (test-false "1" (thunk? proc))
   (test-false "4" (thunk? proc1))
   (test-false "1.1" (thunk? proc3))
   (test-false "2.2" (thunk? proc4))))

(testeez
 "sort-list"
 (test/equal "s1" (sort-list '(3 1 4 4) <) '(1 3 4 4))
 (test/equal "s2" (sort-list '("a" "bc" "b" "z") string>?) '("z" "bc" "b" "a")))

(testeez
 "and/or-map"
 (test-true "and-empty" (and-map (lambda (x) (not x)) '()))
 (test/eqv "and" (and-map (lambda (x) x) '(2 #t 3)) 3)
 (test/eqv "and" (and-map (lambda (x) x) '(#t #t 3 4)) 4)
 (test-false "and" (and-map (lambda (x) (< x 10)) '(3 4 5 13 1)))
 (test-false "or-empty" (or-map (lambda (x) (not x)) '()))
 (test-true "or" (or-map (lambda (x) (not x)) '(#t #t #t #f #t)))
 (test/eqv "or" (or-map (lambda (x) (> x 5)) '(1 3 2 3 34 12)) #t))

;;; misc.scm ends here
