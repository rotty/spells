;; misc.scm -- Utilities that don't fit elsewhere
;; arch-tag: 6c79478b-4740-4db7-a6bf-acd915bf8fc4

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

;;@ Efficiently sort the list @1 using the comparison function
;; @2. Stability is not required.
(define (sort-list lst cmpf) (proc-to-be-defined))

;;@ Sleep @1 seconds.
(define (sleep-seconds t) (proc-to-be-defined))

;;@ Test whether @1 is a procedured that can be called without
;; arguments.
(define (thunk? p) (proc-to-be-defined))

;;@ Returns the value of the environment variable @1 or #f if the
;; variable is not set.
(define (lookup-environment-variable str) (proc-to-be-defined))

;;@ Returns the `unspecific' value, as normally returned by e.g. code
;; @code{(if #f #f)}.
(define (unspecific) (proc-to-be-defined))

;;@ Returns the end-of-file object.
(define eof-object (proc-to-be-defined))

;;; misc.scm ends here
