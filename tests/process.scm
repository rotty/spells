;; process.scm -- unit tests for process.scm
;; arch-tag: 1a073af6-85ae-4ca1-86f1-03a7bbaae8f4

;; Copyright (C) 2005-2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Fri May 20, 2005 23:22

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

(define (lines . strings)
  (values 0 #f (string-join strings (string #\newline) 'suffix)))

(testeez
 "run-process"
 (test/equal "echo"
   (run-process/string #f "echo" "foo")
   (lines "foo"))
 (test/equal "echo -e"
   (run-process/string #f "echo" "-e" "foo\\nbar")
   (lines "foo" "bar"))
 (test/equal "echo -e (lines)"
   (run-process/lines #f "echo" "-e" "foo\\nbar")
   '("foo" "bar"))
 (test/equal "echo (sexps)"
   (run-process/sexps #f "echo" "(hello world)")
   '((hello world)))
 
 (test/equal "env"
   (run-process/string '(("FOO" . "bar")) "/usr/bin/env")
   (lines "FOO=bar")))

;;; process.scm ends here
