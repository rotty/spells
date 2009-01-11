;;; compat.mzscheme.sls --- MzScheme hash tables

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>
;;               2005, 2007 Jose Antonio Ortega <jao@gnu.org>

;; Authors: Jose Antonio Ortega <jao@gnu.org>
;;          Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Sun May 29, 2005 23:41

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

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
       ((eqv) (make-hasheqv))
       ((equal) (make-hash))
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
