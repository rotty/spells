;; table.scm -- unit tests for table.scm
;; arch-tag: 442f011a-faee-40ef-a898-406088ce7b72

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Fri May 20, 2005 22:19

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

(testeez
 "constructors"
 (test-define "v" v-table (make-table 'eqv))
 (test-define "s" s-table (make-table 'equal))
 (test-define "q" q-table (make-table 'eq))
 (test-define "s" s-table1 (make-table))
 (test-true "table?" (table? v-table))
 (test-true "table?" (table? s-table))
 (test-true "table?" (table? q-table))
 (test-true "table?" (table? s-table1))
 (test-false "table?" (table? "foo")))

(let ((table (make-table 'eq)))
  (testeez
   "table lookup (eq? 1)"
   (test-eval "set!" (table-set! table 'foo 2))
   (test-eval "set!" (table-set! table 'bar 3))
   (test-eval "set!" (table-set! table 'qux 5))
   (test/eqv "foo" (table-ref table 'foo) 2)
   (test/eqv "bar" (table-ref table 'bar) 3)
   (test/eqv "qux" (table-ref table 'qux) 5))
  (testeez
   "table lookup (eq? 2)"
   (test-eval "set!" (table-set! table 'foo #f))
   (test-eval "set!" (table-set! table 'bar 8))
   (test-eval "set!" (table-set! table 'qux #f))
   (test/eqv "foo" (table-ref table 'foo) #f)
   (test/eqv "bar" (table-ref table 'bar) 8)
   (test/eqv "qux" (table-ref table 'qux) #f))
  (testeez
   "failure-thunk"
   (test-define "ts" ts (lambda () "ts"))
   (test-define "tn" tn (lambda () 23))
   (test/equal "string" (table-ref table 'baz ts) (ts))
   (test/eqv "number" (table-ref table 'baz tn) (tn))
   (test/eqv "found" (table-ref table 'bar tn) 8)
   (test/eqv "found2" (table-ref table 'bar ts) 8)))

(let ((table (make-table 'equal)))
  (testeez
   "table lookup (equal? 1)"
   (test-eval "set!" (table-set! table 'foo 2))
   (test-eval "set!" (table-set! table 'bar 3))
   (test-eval "set!" (table-set! table 'qux 5))
   (test/eqv "foo" (table-ref table 'foo) 2)
   (test/eqv "bar" (table-ref table 'bar) 3)
   (test/eqv "qux" (table-ref table 'qux) 5))
  (testeez
   "table lookup (equal? 2)"
   (test-eval "set!" (table-set! table 'foo #f))
   (test-eval "set!" (table-set! table 'bar 8))
   (test-eval "set!" (table-set! table 'qux #f))
   (test/eqv "foo" (table-ref table 'foo) #f)
   (test/eqv "bar" (table-ref table 'bar) 8)
   (test/eqv "qux" (table-ref table 'qux) #f))
  (testeez
   "failure-thunk (equal?)"
   (test-define "ts" ts (lambda () "ts"))
   (test-define "tn" tn (lambda () 23))
   (test/equal "string" (table-ref table 'baz ts) (ts))
   (test/eqv "number" (table-ref table 'baz tn) (tn))
   (test/eqv "found" (table-ref table 'bar tn) 8)
   (test/eqv "found2" (table-ref table 'bar ts) 8)))

(let ((table (make-table))
      (table2 (make-table))
      (result #t))
  (testeez
   "walk"
   (test-eval "add" (table-set! table "a" 1))
   (test-eval "add" (table-set! table "b" 1))
   (test-eval "add" (table-set! table "c" 1))
   (test-eval "walk" (table-walk table
                                 (lambda (k v) (table-set! table2 k v))))
   (test-eval "walk2"
              (table-walk table2
                          (lambda (k v)
                            (set! result
                                  (and result
                                       (equal? (table-ref table k) v))))))
   (test-true "result" result)))

;;; named-args.scm ends here
