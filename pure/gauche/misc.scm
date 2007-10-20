;; misc.scm -- Misc utilites for Gauche
;; arch-tag: 8a9e4047-8b99-470f-8434-42cca2087944

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Jun 12, 2005 22:09

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

(define sort-list sort)
(define sleep-seconds sys-sleep)
(define (thunk? p) (and (procedure? p) (procedure-arity-includes? p 0)))


;;; misc.scm ends here
