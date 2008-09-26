;; file.scm -- unit tests for file.scm
;; arch-tag: aaeb0020-2f1a-11d9-a288-00404513c0a4

;; Copyright (C) 2004, 2005-2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Fri Nov 05, 2004 12:05

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

;; This presumes unix namestrings
(testeez
 "Path name handling"

 (test/equal "make-pathname"
   (x->namestring (make-pathname #f '("foo" "bar") "baz"))
   "foo/bar/baz")
 (test/equal "make-pathname"
   (x->namestring (make-pathname '/ '("foo") "bar"))
   "/foo/bar")
 (test/equal "make-pathname"
   (x->namestring (make-pathname #f '() #f))
   "."))

;;; file.scm ends here
