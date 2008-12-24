;;; foreign.scm --- Unit tests for spells.foreign

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Code written towards the GNU C Library (glibc) and will probably
;; only work on GNU/Linux (but should be easily adaptable).

;;; Code:

;; You will probably have to tweak this on a non-GNU/Linux system.
(define *libc-path* "/lib/libc.so.6")

(testeez "callout"
  (test-define "libc" libc (dlopen *libc-path*))
  (test-false "check handle" (eqv? libc #f))
  (test-define "dlsym atoi" atoi-ptr (dlsym libc "atoi"))
  (test-false "check sym" (eqv? atoi-ptr #f))
  (test-define "test str (num)" num-utf8z-ptr (string->utf8z-ptr "424242"))
  (test/equal "call atoi"
    (((make-c-callout 'int '(pointer)) atoi-ptr) num-utf8z-ptr)
    424242))
