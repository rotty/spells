;; util.table.scm -- MzScheme hash tables
;; arch-tag: 432F12DC-EE68-4BF2-B5DE-3236147A33C5

;; Copyright (C) 2005 by Free Software Foundation, Inc.

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

(#%require (only mzscheme
                 make-hash-table hash-table?
                 hash-table-get hash-table-put!))

(define make-table
  (case-lambda
    (() (make-table 'equal))
    ((type)
     (case type
       ((eq) (make-hash-table))
       ((eqv equal) (make-hash-table 'equal))
       (else (error "invalid hash table type"))))))

(define table? hash-table?)

(define table-ref
  (case-lambda
    ((table key) (table-ref table key (lambda () #f)))
    ((table key failure-thunk) (hash-table-get table key failure-thunk))))

(define (table-set! table key value)
  (if value
      (hash-table-put! table key value)
      (hash-table-remove! table key)))

(define table-walk hash-table-for-each)


;;; util.table.scm ends here
