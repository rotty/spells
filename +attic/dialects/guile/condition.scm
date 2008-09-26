;; util.condition.scm -- Dialect specific file for Guile
;; arch-tag: 2042dda6-c591-41d1-af35-93630700a38d

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 21, 2005 03:05

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

(scmxlate-include "srfi-34.scm")
(scmxlate-include "srfi-35.scm")

;; kludge around guile's lousy macro+module support
(export type-field-alist->condition guard-aux)

;;; util.condition.scm ends here
