;; util.table.scm -- Dialect specific file for Guile
;; arch-tag: 7f10adab-e1ae-4f5f-80b6-88b72c50518e

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 20, 2005 21:37

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Comentary:

;;@ Provide a hashtable interface

;;; Code:

(define table-type (make-record-type "table" '(tbl kind)))
(define really-make-table (record-constructor table-type))
(define table-tbl (record-accessor table-type 'tbl))
(define table-kind (record-accessor table-type 'kind))

(define table? (record-predicate table-type))

(define make-table
  (case-lambda
    (()
     (make-table 'equal))
    ((compare)
     (let ((kind (cond ((eq? compare 'eq)      'q)
                       ((eq? compare 'eqv)     'v)
                       ((eq? compare 'equal)   'default)
                       (else
                        (error "invalid compare function for hash table")))))
       (really-make-table (make-hash-table 53) kind)))))

(define table-ref
  (let ((magic-value '(magic)))
    (case-lambda
      ((table key) (table-ref table key default-failure-thunk))
      ((table key failure-thunk)
       (let ((tbl (table-tbl table)))
         (let ((value (case (table-kind table)
                        ((q)  (hashq-ref tbl key magic-value))
                        ((v)  (hashv-ref tbl key magic-value))
                        (else (hash-ref tbl key magic-value)))))
           (if (eq? value magic-value) (failure-thunk) value)))))))

(define (table-set! table key value)
    (let ((tbl (table-tbl table)))
      (case (table-kind table)
        ((q)  (hashq-set! tbl key value))
        ((v)  (hashv-set! tbl key value))
        (else (hash-set! tbl key value)))))

(define (table-walk table proc)
  (hash-fold (lambda (key value ign) (proc key value))
             #f
             (table-tbl table)))


;;; util.table.scm ends here
