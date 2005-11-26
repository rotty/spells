;; process.scm -- unit tests for process.scm
;; arch-tag: 1a073af6-85ae-4ca1-86f1-03a7bbaae8f4

;; Copyright (C) 2005 by Free Software Foundation, Inc.

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
 "exec-process"
 (test/equal "echo" (run-process/string "echo" "foo") (lines "foo")))

;;; process.scm ends here
