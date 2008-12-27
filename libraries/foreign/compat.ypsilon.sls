;;; compat.ypsilon.sls --- foreign compat library for Ypsilon

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells foreign compat)
  (export make-pointer-c-getter make-pointer-c-setter

          pointer? null-pointer null-pointer?
          pointer=?
          pointer+

          make-c-callout
          make-c-callback

          malloc free memcpy memset
          
          dlopen dlsym dlclose dlerror)
  (import (rnrs)
          (spells foreign config)
          (spells alist)
          (core)
          (ypsilon ffi))

  (define (todo-proc who)
    (lambda args
      (error who "please implement me!")))

  (define (make-pointer-c-getter sym)
    (define (bv-ref sym)
      (case sym
        ((char)          bytevector-c-int8-ref)
        ((uchar)         bytevector-c-uint8-ref)
        ((short)         bytevector-c-short-ref)
        ((ushort)        bytevector-c-unsigned-short-ref)
        ((int)           bytevector-c-int-ref)
        ((uint)          bytevector-c-unsigned-int-ref)
        ((long)          bytevector-c-long-ref)
        ((ulong)         bytevector-c-unsigned-long-ref)
        ((llong int64)   bytevector-c-int64-ref)
        ((ullong uint64) bytevector-c-uint64-ref)
        ((float)         bytevector-c-float-ref)
        ((double)        bytevector-c-double-ref)
        ((pointer)       bytevector-c-void*-ref)
        ((int8)          bytevector-c-int8-ref)
        ((uint8)         bytevector-c-uint8-ref)
        ((int16)         bytevector-c-int16-ref)
        ((uint16)        bytevector-c-uint16-ref)
        ((int32)         bytevector-c-int32-ref)
        ((uint32)        bytevector-c-uint32-ref)
        (else #f)))
    (cond ((or (bv-ref sym)
               (bv-ref (resolve-alias sym)))
           => (lambda (ref)
                (lambda (ptr offset)
                  (let ((mapped-bv (make-bytevector-mapping ptr (+ offset 8))))
                    (ref mapped-bv offset)))))
          (else
           (error 'make-pointer-c-getter "invalid type" sym))))
  
  (define (make-pointer-c-setter sym)
    (define (bv-setter sym)
      (case sym
        ((char uchar)    bytevector-c-int8-set!)
        ((short ushort)  bytevector-c-short-set!)
        ((int uint)      bytevector-c-int-set!)
        ((long ulong)    bytevector-c-long-set!)
        ((llong
          int64
          ullong
          uint64)        bytevector-c-int64-set!)
        ((float)         bytevector-c-float-set!)
        ((double)        bytevector-c-double-set!)
        ((pointer)       bytevector-c-void*-set!)
        ((int8 uint8)    bytevector-c-int8-set!)
        ((int16 uint16)  bytevector-c-int16-set!)
        ((int32 uint32)  bytevector-c-int32-set!)
        (else #f)))
    (cond ((or (bv-setter sym)
               (bv-setter (resolve-alias sym)))
           => (lambda (setter)
                (lambda (ptr offset val)
                  (let ((mapped-bv (make-bytevector-mapping ptr (+ offset 8))))
                    (setter mapped-bv offset val)))))
          (else
           (error 'make-pointer-c-setter "invalid type" sym))))
  
  (define (pointer? thing)
    (and (integer? thing) (exact? thing)))
  
  (define (null-pointer) 0)
  
  (define (null-pointer? thing)
    (and (pointer? thing) (= thing 0)))
  
  (define (pointer=? p1 p2)
    (unless (and (pointer? p1) (pointer? p2))
      (error 'pointer=? "invalid arguments" p1 p2))
    (= p1 p2))
  
  (define (pointer+ p offset)
    (unless (and (pointer? p) (integer? offset) (exact? offset))
      (error 'pointer+ "invalid arguments" p offset))
    (+ p offset))

  (define (sized-type ctype signed?)
    (case (c-type-sizeof ctype)
      ((1)  (if signed? 'int8 'uint8))
      ((2)  (if signed? 'int16 'uint16))
      ((4)  (if signed? 'int32 'uint32))
      ((8)  (if signed? 'int64 'uint64))
      (else
       (assertion-violation 'c-type-aliases
                            "unexpected return value from c-type-sizeof"
                            ctype))))

  (define (sized-types-aliases)
    (map (lambda (ctype)
            (let ((signed? (memq ctype '(char short int long llong))))
              (cons (sized-type ctype signed?) ctype)))
          '(char uchar short ushort int uint long ulong llong ullong)))

  (define (other-types-aliases)
    `((fpointer . pointer)

      ;; These are some lame aliases, due to Ypsilon not providing
      ;; many types to begin wth
      (ulong    . int)
      (long     . int)
      
      (size_t . ,(sized-type 'size_t #f))
      (ssize_t . ,(sized-type 'ssize_t #t))
      ;; we assume time_t to be a signed integer type; this true at
      ;; least on glibc systems
      (time_t . ,(sized-type 'time_t #t))))

  (define c-type-aliases (append (sized-types-aliases) (other-types-aliases)))

  (define (resolve-alias ctype)
    (cond ((assq-ref c-type-aliases ctype)
           => (lambda (alias)
                (or (resolve-alias alias)
                    alias)))
          (else #f)))

  (define (c-args-caster arg-types)
    (let ((ats (map (lambda (at)
                      (or (resolve-alias at) at)) arg-types)))
      (lambda (args)
        (let loop ((i 0)
                   (as args)
                   (ats ats)
                   (casted-args '()))
          (cond ((and (null? as) (null? ats))
                 (reverse casted-args))
                ((null? as)
                 (error 'c-callout "too few arguments" args arg-types))
                ((null? ats)
                 (error 'c-callout "too many argumens" args arg-types))
                (else
                 (let ((a (car as)))
                   (loop (+ i 1)
                         (cdr as)
                         (cdr ats)
                         (cons
                          (case (car ats)
                            ((int)     (c-argument c-callout i int a))
                            ((pointer) (c-argument c-callout i void* a))
                            (else
                             (error 'make-c-callout
                                    "cannot handle argument type" (car ats))))
                          casted-args)))))))))
  
  (define (make-c-callout ret-type arg-types)
    (define (rt-stub rt)
      (case rt
        ((void)     call-shared-object->void)
        ((int)      call-shared-object->int)
        ((double)   call-shared-object->double)
        ((pointer)  call-shared-object->intptr)
        (else       #f)))
    (let ((stub (or (rt-stub ret-type)
                    (rt-stub (resolve-alias ret-type))
                    (error 'make-c-callout "invalid return type" ret-type)))
          (cast-args (c-args-caster arg-types)))
      (lambda (ptr)
        (lambda args
          (apply stub ptr (cast-args args))))))

  (define (make-c-callback ret-type arg-types)
    (unless (memq ret-type '(int void))
      (error 'make-c-callback
             "Ypsilon only supports int and void return types"
             arg-types))
    (unless (for-all (lambda (at)
                       (memq at '(int pointer)))
                     arg-types)
      (error 'make-c-callback
             "Ypsilon only supports int and pointer argument types"
             arg-types))
    (let ((n-args (length arg-types))
          (n-rvs (if (eq? ret-type 'void) 0 1)))
      (lambda (proc)
        (make-callback n-rvs n-args proc))))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function libc "C library" ret __stdcall name args)))))
  
  (define libc (load-shared-object
                (cond (on-linux "libc.so.6")
                      (else
                       (error 'libc "unsupported system")))))
  
  (define-function void* malloc (int)) ;; how to get at size_t?
  (define-function void  free (void*))
  
  (define memcpy
    (case-lambda
      ((p1 offset1 p2 offset2 count)
       (cond ((and (pointer? p1) (bytevector? p2))
              (let ((p1-bv (make-bytevector-mapping (+ p1 offset1) count)))
                (bytevector-copy! p2 offset2 p1-bv 0 count)))
             ((and (bytevector? p1) (pointer? p2))
              (let ((p2-bv (make-bytevector-mapping (+ p2 offset2) count)))
                (bytevector-copy! p2-bv 0 p1 offset1 count)))
             (else
              (error 'memcpy "need pointer and bytevector" p1 p2)))
       p1)
      ((p1 p2 count)
       (memcpy p1 0 p2 0 count))))
  
  (define memset (todo-proc 'memset))

  (define *dlerror* #f)
  
  (define dlopen
    (case-lambda
      ((lib-name lazy? global?)
       (guard (c (#t (set! *dlerror* c)))
         (let ((result (load-shared-object lib-name)))
           (set! *dlerror* #f)
           result)))
      ((lib-name)
       (dlopen lib-name #f #f))
      (()
       (dlopen #f #f #f))))

  (define (dlsym lib str)
    (lookup-shared-object lib str))
  
  (define dlclose (todo-proc 'dlclose))
  
  (define (dlerror)
    *dlerror*)
  
)
