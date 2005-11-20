;; misc.scm -- Utilities that don't fit elsewhere
;; arch-tag: ee1d43ab-e7af-4e5a-818f-1c57e96aba35

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Jun 12, 2005 18:50

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

;;; Comentary:

;; Miscellaneous utility functions.

;;; Code:

;;@ Apply @1 to @2 (like map) and apply @code{and} to the
;; resulting list.
(define (and-map proc lst)
  (let loop ((lst lst) (res #t))
    (cond ((null? lst) res)
          (res (loop (cdr lst) (and res (proc (car lst)))))
          (else #f))))

;;@ Apply @1 to @2 (like map) and apply @code{or} to the
;; resulting list.
(define (or-map proc lst)
  (let loop ((lst lst) (res #f))
    (cond ((null? lst) res)
          (res res)
          (else (loop (cdr lst) (or res (proc (car lst))))))))


;;@ The identity function, returning @1.
(define (identity x) x)

;;; misc.scm ends here
