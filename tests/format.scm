;; misc.scm -- Unit tests for spells.util.misc
;; arch-tag: 0e9b49b6-2ba5-4cf7-8154-899bd365dde3

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 30, 2005 17:38

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

(testeez
 "string"
 (test/equal "~a/strings" (format #f "a-~a-~a-~s" "fooish" "bar" 1) "a-fooish-bar-1")
 (test/equal "~s/symbols" (format #f "a-~s-~s-~s" 'fooish 'bar 2) "a-fooish-bar-2")
 (test/equal "~s/string" (format #f "a-~s-~s-~s" "fooish" 'bar 3) "a-\"fooish\"-bar-3"))

;;; format.scm ends here
