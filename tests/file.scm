;; file.scm -- unit tests for file.scm
;; arch-tag: aaeb0020-2f1a-11d9-a288-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
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

(testeez
 "File name handling"
 (test/equal "normalize-path" (normalize-path "foo") "foo")
 (test/equal "normalize-path" (normalize-path "/foo") "/foo")
 (test/equal "normalize-path" (normalize-path "foo/bar/baz") "foo/bar/baz")
 (test/equal "normalize-path" (normalize-path "/foo/bar/") "/foo/bar")
 (test/equal "normalize-path" (normalize-path ".") ".")
 (test/equal "normalize-path" (normalize-path "./foo") "foo")
 (test/equal "normalize-path" (normalize-path "bar/././foo/") "bar/foo")
 (test/equal "normalize-path" (normalize-path "../foo") "../foo")
 (test/equal "normalize-path" (normalize-path "..//../foo") "../../foo")
 (test/equal "normalize-path"
             (normalize-path "bar////baz/../../qux/../foo") "foo")

 (test/equal "make-path" (make-path "foo" "bar" "baz") "foo/bar/baz")
 (test/equal "make-path" (make-path "/foo" "bar") "/foo/bar")
 (test/equal "make-path" (make-path "foo" "/root" "bar") "foo/root/bar")
 (test/equal "make-path" (make-path "foo" "bar/") "foo/bar")
 (test/equal "make-path" (make-path "/foo" "bar" "quz/") "/foo/bar/quz")
 (test/equal "make-path"
             (make-path "foo" "bar" "quz/foo.tex") "foo/bar/quz/foo.tex")
 (test/equal "make-path" (make-path "/foo" "bar/baz" "qux.scm")
                  "/foo/bar/baz/qux.scm")

 (test-true "absolute-path?" (absolute-path? "/foo/.././bar"))
 (test-false "absolute-path?" (absolute-path? "foo/.././bar"))
 (test-false "absolute-path?" (absolute-path? "foo/..//bar"))

 (test-false "dot-or-dotdot?" (dot-or-dotdot? "./foo"))
 (test-false "dot-or-dotdot?" (dot-or-dotdot? "..."))
 (test-true "dot-or-dotdot?" (dot-or-dotdot? ".."))
 (test-true "dot-or-dotdot?" (dot-or-dotdot? ".."))

 (test/equal "file-extension" (file-extension "foo/.././bar/e.foo") "foo")
 (test/equal "file-extension" (file-extension "foo/.././bar/e.foo/") "")
 (test/equal "file-extension" (file-extension "foo") "")
 (test/equal "file-extension" (file-extension "foo.bar") "bar")
 (test/equal "file-extension" (file-extension "foo.") "")
 (test/equal "file-extension" (file-extension ".foo") "")
 (test/equal "file-extension" (file-extension ".foo/./") "")

 (test/equal "file-basename" (file-basename "foo/.././bar/e.foo") "e.foo")
 (test/equal "file-basename" (file-basename "foo/.././bar/e.foo/" "foo") "e.")
 (test/equal "file-basename" (file-basename "foo") "foo")
 (test/equal "file-basename" (file-basename "foo.bar" "o.bar") "fo")
 (test/equal "file-basename" (file-basename "foo.") "foo.")
 (test/equal "file-basename" (file-basename "//...foo") "...foo")
 (test/equal "file-basename" (file-basename "/.foo/./a/.") ".")

 (test/equal "file-dirname" (file-dirname "foo/.././bar/e.foo") "foo/.././bar")
 (test/equal "file-dirname" (file-dirname "foo/.././bar/e.foo/") "foo/.././bar")
 (test/equal "file-dirname" (file-dirname "foo") ".")
 (test/equal "file-dirname" (file-dirname "foo.bar") ".")
 (test/equal "file-dirname" (file-dirname "/foo.") "/")
 (test/equal "file-dirname" (file-dirname "a//...foo") "a")
 (test/equal "file-dirname" (file-dirname "/.foo/./a/.") "/.foo/./a")
 (test/equal "file-dirname" (file-dirname "/") "/")

 (test/equal "append-extension" (append-extension "foo" "bar") "foo.bar")
 (test/equal "append-extension" (append-extension "foo.bar" "bar") "foo.bar")
 (test/equal "append-extension" (append-extension "a/b/foo" "bar") "a/b/foo.bar")
 (test/equal "append-extension"
             (append-extension "/foo/foo.bar" "bar") "/foo/foo.bar")

 (test-define "Current dir" cur (current-directory))
 (test/equal "current-directory"
             (with-current-directory "/" (current-directory)) "/")
 (test/equal "current-directory" (current-directory) cur))


;;; file.scm ends here
