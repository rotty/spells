;; ascii.scm -- ASCII encoding
;; arch-tag: 75433a58-dc9a-469c-b127-8d1b4edc7124

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 31, 2005 16:25

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Comentary:

;;; Code:

;;@ Converts an ASCII code (integer) into the corresponding
;;  character.
(define (ascii->char n) (proc-to-be-defined))

;;@ Converts the character @1 into the corresponding ASCII code.
(define (char->ascii c) (proc-to-be-defined))

;;@ Upper bound of the integer range accepted by ascii->char.
(define ascii-limit 128)

;;@ List of integers that are considered white-space.
(define ascii-whitespaces '(32 10 9 12 13)) ;space linefeed tab page return

;;; ascii.scm ends here
