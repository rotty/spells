;; util.table.scm -- Dialect specific file for Scheme48
;; arch-tag: f51ff1e6-d91f-496b-90a1-cd0745c11416

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 20, 2005 21:35

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

;; Slightly extended version of the s48 interface

;;; Code:

(define make-table
  (case-lambda
    (() (make-table 'equal))
    ((compare)
     (case compare
       ((eq)    ((s48:make-table-maker eq? s48:default-hash-function)))
       ((eqv)   ((s48:make-table-maker eq? s48:default-hash-function)))
       ((equal) ((s48:make-table-maker equal? generic-hash-function)))
       (else    (error "invalid compare function for hash table"))))))

(define table? s48:table?)
(define table-set! s48:table-set!)
(define (table-walk table proc)
  (s48:table-walk proc table))

(define the-false-value (list #f))

(define (generic-hash-function obj)
  (cond ((string? obj)
         (s48:string-hash obj))
        (else
         (s48:default-hash-function obj))))

(define table-ref
  (case-lambda
    ((table key) (table-ref table key default-failure-thunk))
    ((table key failure-thunk)
     (let ((value (s48:table-ref table key)))
       (cond ((eq? value the-false-value) #f)
             ((eq? value #f) (failure-thunk))
             (else value))))))

;;; util.table.scm ends here
