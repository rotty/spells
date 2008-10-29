;; pathname.scm -- unit tests for (spells pathname)
;; arch-tag: aaeb0020-2f1a-11d9-a288-00404513c0a4

;; Copyright (C) 2004, 2005-2006, 2008 by Free Software Foundation, Inc.

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

(testeez "pathname comparison"
  (test-true "empty" (pathname=? (make-pathname #f '() #f) (make-pathname #f '() #f)))
  (test-false "one empty" (pathname=? (make-pathname #f '() #f) (make-pathname #f '() "foo"))))

(testeez "pathname s-expr parsing"
  (test/equiv "string"
    (x->pathname "foo")
    (make-pathname #f '() "foo")
    (pathname=?))
  (test/equiv "dirlist, no filename"
    (x->pathname '(("foo" "bar")))
    (make-pathname #f '("foo" "bar") #f)
    (pathname=?))
  (test/equiv "dirlist, nonempty"
    (x->pathname '(("foo" "bar") "baz"))
    (make-pathname #f '("foo" "bar") "baz")
    (pathname=?))
  (test/equiv "dirlist, empty"
    (x->pathname '(() "baz"))
    (make-pathname #f '() "baz")
    (pathname=?)))

;; The rest of the tests presume unix namestrings
(testeez "namestring conversion"

 (test/equal "relative"
   (x->namestring (make-pathname #f '("foo" "bar") "baz"))
   "foo/bar/baz")
 (test/equal "absolute"
   (x->namestring (make-pathname '/ '("foo") "bar"))
   "/foo/bar")
 (test/equal "empty"
   (x->namestring (make-pathname #f '() #f))
   ".")
 (test/equal "dir, but no file"
   (x->namestring (make-pathname #f '("foo") #f))
   "foo/"))

(testeez "file type parsing"
  (test/equiv "one type"
    (x->pathname "foo.scm")
    (make-pathname #f '() (make-file "foo" "scm"))
    (pathname=?)))

;;; pathname.scm ends here
