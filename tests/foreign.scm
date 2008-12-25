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

(define N 1000)

(testeez "pointers"
  (test-true "null pointer is a pointer" (pointer? (null-pointer)))
  (test/equiv "pointer+ sanity"
    (pointer+ (null-pointer) 777)
    (pointer+ (pointer+ (null-pointer) 666) 111)
    (pointer=?)))

(testeez "malloc/free, mem access"
  (test/equal "stress"
    (let loop ((i 0) (sum 0))
      (if (>= i N)
          sum
          (let ((mem (malloc 4)))
            (pointer-uint32-set! mem 0 (* i 7))
            (let ((v (pointer-uint32-ref mem 0)))
              (free mem)
              (loop (+ i 1) (+ sum v))))))
    (/ (* N (- N 1) 7) 2)))

(testeez "callout"
  (test-define "libc" libc (dlopen *libc-path*))
  (test-false "check handle" (eqv? libc #f))
  (test-define "dlsym atoi" atoi-ptr (dlsym libc "atoi"))
  (test-false "check sym" (eqv? atoi-ptr #f))
  (test-define "test str (num)" num-utf8z-ptr (string->utf8z-ptr "424242"))
  (test/equal "call atoi"
    (((make-c-callout 'int '(pointer)) atoi-ptr) num-utf8z-ptr)
    424242))

(testeez "callback"
  (test-define "bsearch-ptr" bsearch-ptr (dlsym (dlopen *libc-path*) "bsearch"))
  (test-define "bsearch" bsearch
    ((make-c-callout 'pointer '(pointer pointer size_t size_t pointer)) bsearch-ptr))
  (test-define "data" data (let ((mem (malloc (* 5 4))))
                             (pointer-uint32-set! mem (* 0 4) 7)
                             (pointer-uint32-set! mem (* 1 4) 11)
                             (pointer-uint32-set! mem (* 2 4) 23)
                             (pointer-uint32-set! mem (* 3 4) 31)
                             (pointer-uint32-set! mem (* 4 4) 37)
                             mem))
  (test-define "comparator" cmp ((make-c-callback 'int '(pointer pointer))
                                 (lambda (p1 p2)
                                   (- (pointer-uint32-ref p1 0)
                                      (pointer-uint32-ref p2 0)))))
  (test-define "key cell" key (malloc 4))
  (test/equiv "23 (hit)"
    (begin
      (pointer-uint32-set! key 0 23)
      (bsearch key data 5 4 cmp))
    (pointer+ data (* 2 4))
    (pointer=?))
  (test/equiv "10 (miss)"
    (begin
      (pointer-uint32-set! key 0 10)
      (bsearch key data 5 4 cmp))
    (null-pointer)
    (pointer=?)))
