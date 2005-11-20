;; condition.scm -- Conditions
; arch-tag: 4f83ff72-688f-49eb-890f-b429a890276f

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Tue Nov 08, 2005 20:26

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

;;; Code:

(define-condition-type &parser-error &error
  parser-error?
  (port parser-error-port))

(define-condition-type &end-of-input &condition
  end-of-input?
  (port end-of-input-port))

;;; condition.scm ends here

