;; table.scm -- unit tests for (spells table)

;; Copyright (C) 2005, 2009 by Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Fri May 20, 2005 22:19

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

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
