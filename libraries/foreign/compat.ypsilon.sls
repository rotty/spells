;;; compat.ypsilon.sls --- FFI compat library for Ypsilon

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

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
          (xitomatl srfi let-values)
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
                            ((uint)    (c-argument c-callout i int a)) ; ?
                            ((long)    (c-argument c-callout i long a))
                            ((pointer) (c-argument c-callout i void* a))
                            ((double)  (c-argument c-callout i double a))
                            ((float)   (c-argument c-callout i float a))
                            (else
                             (error 'make-c-callout
                                    "cannot handle argument type" (car ats))))
                          casted-args)))))))))
  
  (define (make-c-callout ret-type arg-types)
    (define (rt-stub+cast rt)
      (case rt
        ((void)     (values call-shared-object->void   #f))
        ((int)      (values call-shared-object->int    #f))
        ((uint)     (values call-shared-object->int    int->unsigned-int))
        ((long)     (values call-shared-object->intptr #f))
        ((ulong)    (values call-shared-object->intptr intptr->uintptr))
        ((double)   (values call-shared-object->double #f))
        ((pointer)  (values call-shared-object->intptr #f))
        (else       (values #f                         #f))))
    (let ((ret-alias (resolve-alias ret-type)))
      (let-values (((stub cast-ret) (rt-stub+cast (or ret-alias ret-type)))
                   ((cast-args) (c-args-caster arg-types)))
        (unless stub
          (error 'make-c-callout "invalid return type" ret-type))
        (if cast-ret
            (lambda (ptr)
              (lambda (args)
                (cast-ret (apply stub ptr (cast-args args)))))
            (lambda (ptr)
              (lambda args
                (apply stub ptr (cast-args args))))))))

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
  
  (define (memset p v n)
    (let ((p-bv (make-bytevector-mapping p n)))
      (bytevector-fill! p-bv v))
    p)

  (define *dlerror* #f)
  
  (define dlopen
    (case-lambda
      ((lib-name lazy? global?)
       (guard (c (#t (set! *dlerror* c) #f))
         (let ((result (load-shared-object lib-name)))
           (set! *dlerror* #f)
           result)))
      ((lib-name)
       (dlopen lib-name #f #f))
      (()
       ;; Ypsilon doesn't support that yet
       #f)))

  (define (dlsym lib str)
    (lookup-shared-object lib str))
  
  (define dlclose (todo-proc 'dlclose))
  
  (define (dlerror)
    *dlerror*)

  (define int->unsigned-int
    (let ((unsigned-int-mask (- (bitwise-arithmetic-shift 1 (* sizeof:int 8)) 1)))
      (lambda (val) (if (< val 0) (bitwise-and val unsigned-int-mask) val))))
  
  (define intptr->uintptr
    (let ((uintptr-mask (- (bitwise-arithmetic-shift 1 (* sizeof:void* 8)) 1)))
      (lambda (val) (if (< val 0) (bitwise-and val uintptr-mask) val))))

)
