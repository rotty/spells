;;; hash-tables.sls --- SRFI 69, Basic Hash Tables.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Implementation of
;;  @uref{http://srfi.schemers.org/srfi-69/srfi-69.html, SRFI 69},
;;  (Basic hash tables).
(library (spells hash-tables)
  (export
   ;; Type constructors and predicate
   make-hash-table hash-table? alist->hash-table 
   ;; Reflective queries
   hash-table-equivalence-function hash-table-hash-function 
   ;; Dealing with single elements
   hash-table-ref hash-table-ref/default hash-table-set!
   hash-table-delete! hash-table-exists?
   hash-table-update! hash-table-update!/default 
   ;; Dealing with the whole contents
   hash-table-size hash-table-keys hash-table-values hash-table-walk
   hash-table-fold hash-table->alist hash-table-copy hash-table-merge! 
   ;;Hashing
   hash string-hash string-ci-hash hash-by-identity)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs lists)
          (rnrs unicode)
          (rnrs mutable-pairs)
          (rnrs r5rs)
          (only (spells error) make-error-signaller)
          (spells record-types)
          (spells include))

  (define error (make-error-signaller "SRFI-69"))
  
  (include-file ((spells private) srfi-69)))
