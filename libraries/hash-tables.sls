;;; hash-tables.sls --- SRFI 69, Basic Hash Tables.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

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
  
  (include-file ((spells scheme) srfi-69)))
