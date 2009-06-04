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
(define *libc-path* "libc.so.6")

(define N 1000)

(define-test-suite foreign-tests
  "Foreign Function Interface")

(define-test-suite (foreign-tests.pointers foreign-tests)
  "Pointer abstraction")

(define-test-case foreign-tests.pointers null-pointer ()
  (test-equal #t (pointer? (null-pointer))))

(define-test-case foreign-tests.pointers pointer+ ()
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

(define-test-suite (foreign-tests.callouts foreign-tests)
  "Callouts")

(define-test-case foreign-tests.callouts dlopen ()
  (let ((libc (dlopen *libc-path*)))
    (test-equal #f (eqv? libc #f))))

(define-test-case foreign-tests.callouts atoi ()
  (let* ((libc (dlopen *libc-path*))
         (atoi-ptr (dlsym libc "atoi"))
         (num-utf8z-ptr (string->utf8z-ptr "424242")))
    (test-equal #f (eqv? atoi-ptr #f))
    (test-equal 424242
      (((make-c-callout 'int '(pointer)) atoi-ptr) num-utf8z-ptr))))

(define-test-case foreign-tests.callouts realloc ()
  (let* ((libc (dlopen *libc-path*))
         (realloc ((make-c-callout 'pointer '(pointer size_t))
                   (dlsym libc "realloc")))
         ;; need to use libc's free here, as it might differ from the
         ;; one provided by (spells foreign)
         (free ((make-c-callout 'void '(pointer))
                (dlsym libc "free"))))
    (let ((mem (realloc (null-pointer) 1024)))
      (test-equal #t (pointer? mem))
      (free mem))))

(define-test-case foreign-tests.callouts strtoll ()
  (let* ((libc (dlopen *libc-path*))
         (strtoll ((make-c-callout 'llong '(pointer pointer int))
                   (dlsym libc "strtoll")))
         (num-utf8z-ptr (string->utf8z-ptr "-8223372036854775807")))
    (test-equal -8223372036854775807
      (strtoll num-utf8z-ptr (null-pointer) 10))
    (free num-utf8z-ptr)))

(define-test-case foreign-tests callback ()
  (let ((bsearch
         ((make-c-callout 'pointer '(pointer pointer size_t size_t fpointer))
          (dlsym (dlopen *libc-path*) "bsearch")))
        (data
         (let ((mem (malloc (* 5 4))))
           (pointer-uint32-set! mem (* 0 4) 7)
           (pointer-uint32-set! mem (* 1 4) 11)
           (pointer-uint32-set! mem (* 2 4) 23)
           (pointer-uint32-set! mem (* 3 4) 31)
           (pointer-uint32-set! mem (* 4 4) 37)
           mem))
        (cmp
         ((make-c-callback 'int '(pointer pointer))
          (lambda (p1 p2)
            (- (pointer-uint32-ref p1 0)
               (pointer-uint32-ref p2 0)))))
        (key (malloc 4)))
    (test-compare
     pointer=?
     (pointer+ data (* 2 4))
     (begin
       (pointer-uint32-set! key 0 23)
       (bsearch key data 5 4 cmp)))

    (test-compare
     pointer=?
     (null-pointer)
     (begin
       (pointer-uint32-set! key 0 10)
       (bsearch key data 5 4 cmp)))))

(run-test-suite foreign-tests)
