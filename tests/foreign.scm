;;; foreign.scm --- Unit tests for spells.foreign

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Code written towards the GNU C Library (glibc) and will probably
;; only work on GNU/Linux (but should be easily adaptable).

;;; Code:

;; You will probably have to tweak this on a non-GNU/Linux system.
(define *libc-path* "/lib/libc.so.6")

(define N 1000)

(define-test-suite foreign-tests
  "Foreign Function Interface")

(define-test-suite (foreign-tests.pointers foreign-tests)
  "Pointer abstraction")

(define-test-case foreign-tests.pointers null-pointer
  (test-equal #t (pointer? (null-pointer))))

(define-test-case foreign-tests.pointers pointer+
  (test-compare pointer=?
                (pointer+ (null-pointer) 777)
                (pointer+ (pointer+ (null-pointer) 666) 111)))

(define-test-suite (foreign-tests.mem foreign-tests)
  "Memory access, malloc/free")

(define-test-case foreign-tests.mem stress ()
  (test-equal (/ (* N (- N 1) 7) 2)
    (let loop ((i 0) (sum 0))
      (if (>= i N)
          sum
          (let ((mem (malloc 4)))
            (pointer-uint32-set! mem 0 (* i 7))
            (let ((v (pointer-uint32-ref mem 0)))
              (free mem)
              (loop (+ i 1) (+ sum v))))))))

(testeez "callout"
  (test-define "libc" libc (dlopen *libc-path*))
  (test-false "check handle" (eqv? libc #f))
  (test-define "dlsym atoi" atoi-ptr (dlsym libc "atoi"))
  (test-false "check sym" (eqv? atoi-ptr #f))
  (test-define "test str (num)" num-utf8z-ptr (string->utf8z-ptr "424242"))
  (test/equal "call atoi"
    (((make-c-callout 'int '(pointer)) atoi-ptr) num-utf8z-ptr)
    424242)
  (test-define "realloc" realloc ((make-c-callout 'pointer '(pointer size_t))
                                  (dlsym libc "realloc")))
  (test-define "call realloc" mem (realloc (null-pointer) 1024))
  (test-true "pointer?" (pointer? mem))
  (test-eval "free" (free mem)))

(testeez "callback"
  (test-define "bsearch-ptr" bsearch-ptr (dlsym (dlopen *libc-path*) "bsearch"))
  (test-define "bsearch" bsearch
    ((make-c-callout 'pointer '(pointer pointer size_t size_t fpointer)) bsearch-ptr))
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
