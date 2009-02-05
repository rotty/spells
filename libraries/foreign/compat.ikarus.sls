;;; compat.ikarus.sls --- FFI compat library for Ikarus.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(library (spells foreign compat)
  (export make-pointer-c-getter make-pointer-c-setter

          pointer? null-pointer null-pointer?
          pointer=?
          pointer+

          (rename (spells:make-c-callout make-c-callout)
                  (spells:make-c-callback make-c-callback))

          malloc free memcpy memset

          dlopen dlsym dlclose dlerror)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (rnrs lists)
          (rnrs bytevectors)
          (srfi :2 and-let*)
          (spells alist)
          (spells parameter)
          (spells foreign config)
          (spells tracing)
          (ikarus foreign))

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

  (define null-pointer
    (let ((null (integer->pointer 0)))
      (lambda ()
        null)))

  (define (null-pointer? ptr)
    (= (pointer->integer ptr) 0))

  (define (pointer=? p1 p2)
    (= (pointer->integer p1)
       (pointer->integer p2)))

  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n)))

  (define (sized-types-aliases)
    (map (lambda (ctype)
            (let ((signed? (memq ctype '(char short int long llong))))
              (cons (sized-type ctype signed?) ctype)))
          '(char uchar short ushort int uint long ulong llong ullong)))

  (define (other-types-aliases)
    `((fpointer . pointer)
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

  (define (make-pointer-c-getter sym)
    (define (primitive-ref sym)
      (case sym
        ((char)   pointer-ref-c-signed-char)
        ((uchar)  pointer-ref-c-unsigned-char)
        ((short)  pointer-ref-c-signed-short)
        ((ushort) pointer-ref-c-unsigned-short)
        ((int)    pointer-ref-c-signed-int)
        ((uint)   pointer-ref-c-unsigned-int)
        ((long)   pointer-ref-c-signed-long)
        ((ulong)  pointer-ref-c-unsigned-long)
        ((llong)  pointer-ref-c-signed-long-long)
        ((ullong) pointer-ref-c-unsigned-long-long)
        ((float)  pointer-ref-c-float)
        ((double) pointer-ref-c-double)
        ((pointer) pointer-ref-c-pointer)
        (else #f)))
    (or (primitive-ref sym)
        (let ((alias (resolve-alias sym)))
          (or (and alias (primitive-ref alias))
              (error 'make-pointer-c-getter "invalid type" sym)))))

  (define (spells:make-c-callout ret-type arg-types)
    (make-c-callout (type->ikarus-type ret-type)
                    (map type->ikarus-type arg-types)))

  (define (spells:make-c-callback ret-type arg-types)
    (make-c-callback (type->ikarus-type ret-type)
                     (map type->ikarus-type arg-types)))

  (define (type->ikarus-type type)
    (define (prim->ikarus-type prim)
      (case prim
        ((char) 'signed-char)
        ((uchar) 'unsigned-char)
        ((short) 'signed-short)
        ((ushort) 'unsigned-short)
        ((int) 'signed-int)
        ((uint) 'unsigned-int)
        ((long) 'signed-long)
        ((ulong) 'unsigned-long)
        ((float) 'float)
        ((double) 'double)
        ((pointer) 'pointer)
        ((void) 'void)
        (else #f)))
    (or (prim->ikarus-type type)
        (and-let* ((alias (resolve-alias type)))
          (prim->ikarus-type alias))
        (error 'type->ikarus-type "invalid type" type)))

  (define (make-pointer-c-setter sym)
    (define (primitive-set sym)
      (case sym
        ((char)   pointer-set-c-char!)
        ((uchar)  pointer-set-c-char!)
        ((short)  pointer-set-c-short!)
        ((ushort) pointer-set-c-short!)
        ((int)    pointer-set-c-int!)
        ((uint)   pointer-set-c-int!)
        ((long)   pointer-set-c-long!)
        ((ulong)  pointer-set-c-long!)
        ((llong)  pointer-set-c-long-long!)
        ((ullong) pointer-set-c-long-long!)
        ((float)  pointer-set-c-float!)
        ((double) pointer-set-c-double!)
        ((pointer) pointer-set-c-pointer!)
        (else #f)))
    (or (primitive-set sym)
        (let ((alias (resolve-alias sym)))
          (or (and alias (primitive-set alias))
              (error 'make-pointer-c-setter "invalid type" sym)))))

  (define memcpy
    (case-lambda
      ((p1 offset1 p2 offset2 count)
       (cond ((and (pointer? p1) (bytevector? p2))
              (do ((i offset1 (+ i 1))
                   (j offset2 (+ j 1)))
                  ((>= (- i offset1) count))
                (pointer-set-c-char! p1 i (bytevector-u8-ref p2 j))))
             ((and (bytevector? p1) (pointer? p2))
              (do ((i offset1 (+ i 1))
                   (j offset2 (+ j 1)))
                  ((>= (- i offset1) count))
                (bytevector-u8-set! p1 i (pointer-ref-c-unsigned-char p2 j))))
             (else
              (error 'memcpy "need pointer and bytevector" p1 p2)))
       p1)
      ((p1 p2 count)
       (memcpy p1 0 p2 0 count))))

  (define (memset p v n)
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (pointer-set-c-char! p i v))
    p))
