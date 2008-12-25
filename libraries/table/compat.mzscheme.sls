;; util.table.scm -- MzScheme hash tables
;; arch-tag: 432F12DC-EE68-4BF2-B5DE-3236147A33C5

;; Copyright (C) 2005, 2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega <jao@gnu.org>
;; Start date: Sun May 29, 2005 23:41

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
#!r6rs

(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (rnrs base)
          (rnrs control)
          (only (scheme base)
                make-hash make-hasheq make-hasheqv
                hash?
                hash-ref hash-set! hash-remove!
                hash-for-each))

(define make-table
  (case-lambda
    (() (make-hash))
    ((type)
     (case type
       ((eq) (make-hasheq))
       ((eqv equal) (make-hasheqv))
       (else (error "invalid hash table type"))))))

(define table? hash?)

(define table-ref
  (case-lambda
    ((table key) (table-ref table key (lambda () #f)))
    ((table key failure-thunk) (hash-ref table key failure-thunk))))

(define (table-set! table key value)
  (if value
      (hash-set! table key value)
      (hash-set! table key)))

(define table-walk hash-for-each)

)
;;; util.table.scm ends here
