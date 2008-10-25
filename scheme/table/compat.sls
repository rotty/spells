;; table.scm -- Hash tables
;; arch-tag: 7a68d589-df5f-4915-a2cc-5217717ea70e

;; Copyright (C) 2005, 2008 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 20, 2005 21:29

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


(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (rnrs base)
          (rnrs hashtables))
  
  ;;@ Create a hash table. The optional argument @1 can be either
  ;; @code{'eq}, @code{'eqv} or @code{'equal}.
  (define (make-table . type) (proc-to-be-defined))

  ;;@ Table type predicate. Hash tables are a disjoint type.
  (define (table? thing) (proc-to-be-defined))

  ;;@ Lookup @2 in @1. If no value is found for key @2, return the value
  ;; obtained by invoking @3 with no arguments, or #f if the optional
  ;; argument @3 is not specified.
  (define (table-ref table key . failure-thunk) (proc-to-be-defined))

  ;;@ Set the value correspoinding to @2 in @1 to @3.
  (define (table-set! table key value) (proc-to-be-defined))

  ;;@ Call @2 with the key and value of every entry in @1 as arguments.
  (define (table-walk table proc) (proc-to-be-defined)))

;;; table.scm ends here
