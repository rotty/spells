;; util.misc.scm -- Misc utilites for MzScheme
;; arch-tag: b27f4a34-4209-44ab-8586-49fd8c1b8322

;; Copyright (C) 2005, 2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Jun 12, 2005 22:16

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
                 sleep procedure-arity-includes?
                 eof getenv andmap ormap void exit)
           (only (lib "list.ss") quicksort))

(define sort-list quicksort)
(define sleep-seconds sleep)
(define (thunk? p) (and (procedure? p) (procedure-arity-includes? p 0)))
(define and-map andmap)
(define or-map ormap)
(define (eof-object) eof)
(define (identity x) x)
(define (compose f g) (lambda args (f (apply g args))))
(define (unspecific) (void))
(define (scheme-dialect) 'mzscheme)

;;; misc.scm ends here
