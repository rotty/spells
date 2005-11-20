;; ascii.scm -- Dialect specific file for Guile
;; arch-tag: 00d09ef5-461f-4462-9828-8be4d54cd13c

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 31, 2005 17:26

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

;;; Code:

(define (ascii->char n)
  (if (>= n ascii-limit)
      (error "not within ascii range" n)
      (integer->char n)))

(define (char->ascii c)
  (let ((n (char->integer c)))
    (if (>= n ascii-limit)
        (error "not within ascii range" n)
        n)))

;;; ascii.scm ends here
