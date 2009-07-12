;;; foreign.sls --- Foreign function interface.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Foreign function interface.
(library (spells foreign)
  (export c-type-sizeof c-type-alignof c-type-align

          make-pointer-c-getter make-pointer-c-setter
          make-pointer-c-element-getter
          make-pointer-c-element-setter

          pointer?
          null-pointer
          null-pointer?
          pointer=?
          pointer+
          let*-pointers

          make-c-callout make-c-callback

          malloc free memcpy memset

          errno

          dlopen dlsym dlclose dlerror

          pointer-short-ref
          pointer-short-set!
          pointer-ushort-ref
          pointer-ushort-set!
          pointer-int-ref
          pointer-int-set!
          pointer-long-ref
          pointer-long-set!
          pointer-ulong-ref
          pointer-ulong-set!
          pointer-llong-set!
          pointer-llong-ref
          pointer-ullong-set!
          pointer-ullong-ref

          pointer-int8-ref
          pointer-int8-set!
          pointer-uint8-ref
          pointer-uint8-set!
          pointer-uint16-ref
          pointer-uint16-set!
          pointer-uint32-ref
          pointer-uint32-set!
          pointer-uint64-ref
          pointer-uint64-set!

          pointer-ptr-ref
          pointer-ptr-set!

          utf8z-ptr->string
          string->utf8z-ptr
          ->utf8z-ptr/null

          pointer-utf8z-ptr-set!
          pointer-utf8z-ptr-ref

          define-c-callouts)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (rnrs bytevectors)
          (rnrs syntax-case)
          (for (srfi :8 receive) run expand)
          (for (only (srfi :1) append-reverse) expand)
          (for (spells foof-loop) expand)
          (spells foreign compat)
          (spells foreign frozen-bytes)
          (spells foreign config))

  ;; TODO: callout compression (i.e. reusing callouts with the same
  ;; signature)
  (define-syntax define-c-callouts
    (syntax-rules ()
      ((define-callouts shlib (name ret-type c-name arg-types) ...)
       (begin
         (define name ((make-c-callout ret-type arg-types) (dlsym shlib c-name)))
         ...))))

  (define (utf8z-ptr->string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-uint8-ref ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))

  (define (->utf8z-ptr/null who s)
    (cond ((string? s) (string->utf8z-ptr s))
          ((eqv? s #f)
           (null-pointer))
          (else
           (assertion-violation who "invalid argument" s))))

  (define (string->utf8z-ptr s)
    (let* ((bytes (string->utf8 s))
           (bytes-len (bytevector-length bytes))
           (result (malloc (+ bytes-len 1))))
      (memcpy result bytes bytes-len)
      (pointer-uint8-set! result bytes-len 0)
      result))

  (define (pointer-utf8z-ptr-set! ptr i val)
    (pointer-ptr-set! ptr i (if (pointer? val)
                                val
                                (string->utf8z-ptr val))))

  (define (pointer-utf8z-ptr-ref ptr i)
    (let ((utf8z-ptr (pointer-ptr-ref ptr i)))
      (if (null-pointer? utf8z-ptr)
          #f
          (utf8z-ptr->string utf8z-ptr))))

  (define pointer-short-ref   (make-pointer-c-getter 'short))
  (define pointer-short-set!  (make-pointer-c-setter 'short))
  (define pointer-ushort-ref  (make-pointer-c-getter 'ushort))
  (define pointer-ushort-set! (make-pointer-c-setter 'ushort))
  (define pointer-int-ref     (make-pointer-c-getter 'int))
  (define pointer-int-set!    (make-pointer-c-setter 'int))
  (define pointer-uint-ref    (make-pointer-c-getter 'uint))
  (define pointer-uint-set!   (make-pointer-c-setter 'uint))
  (define pointer-long-ref    (make-pointer-c-getter 'long))
  (define pointer-long-set!   (make-pointer-c-setter 'long))
  (define pointer-ulong-ref   (make-pointer-c-getter 'ulong))
  (define pointer-ulong-set!  (make-pointer-c-setter 'ulong))
  (define pointer-llong-ref   (make-pointer-c-getter 'llong))
  (define pointer-llong-set!  (make-pointer-c-setter 'llong))
  (define pointer-ullong-ref  (make-pointer-c-getter 'ullong))
  (define pointer-ullong-set! (make-pointer-c-setter 'ullong))
  
  (define pointer-int8-ref    (make-pointer-c-getter 'int8))
  (define pointer-int8-set!   (make-pointer-c-setter 'int8))
  (define pointer-uint8-ref   (make-pointer-c-getter 'uint8))
  (define pointer-uint8-set!  (make-pointer-c-setter 'uint8))
  (define pointer-uint16-ref  (make-pointer-c-getter 'uint16))
  (define pointer-uint16-set! (make-pointer-c-setter 'uint16))
  (define pointer-uint32-ref  (make-pointer-c-getter 'uint32))
  (define pointer-uint32-set! (make-pointer-c-setter 'uint32))
  (define pointer-uint64-ref  (make-pointer-c-getter 'uint64))
  (define pointer-uint64-set! (make-pointer-c-setter 'uint64))
  (define pointer-ptr-ref     (make-pointer-c-getter 'pointer))
  (define pointer-ptr-set!    (make-pointer-c-setter 'pointer))

  (define (c-type-align ctype n)
    (let ((alignment (c-type-alignof ctype)))
      (+ n (mod (- alignment (mod n alignment)) alignment))))

  (define (make-pointer-c-element-getter type offset bit-offset bits)
    (case type
      ((record union array)
         (lambda (pointer)
           (pointer+ pointer offset)))
      (else
       (let ((ptr-ref (make-pointer-c-getter type)))
         (cond ((and bits bit-offset)
                (let ((end-offset (+ bit-offset bits)))
                  (lambda (pointer)
                    (let ((val (ptr-ref pointer offset)))
                      (bitwise-bit-field val bit-offset end-offset)))))
               (else
                (lambda (pointer) (ptr-ref pointer offset))))))))

  (define (make-pointer-c-element-setter type offset bit-offset bits)
    (define (lose msg . irritants)
      (apply error 'make-pointer-c-element-setter msg irritants))
    (case type
      ((record union array)
       (lose "cannot set compound element" type))
      (else
       (let ((ptr-set (make-pointer-c-setter type)))
         (cond ((and bits bit-offset)
                (let ((end-offset (+ bit-offset bits))
                      (ptr-ref (make-pointer-c-getter type))
                      (mask (bitwise-not (bitwise-arithmetic-shift-left -1 bit-offset))))
                  (lambda (pointer val)
                    (let ((val (ptr-ref pointer offset)))
                      (ptr-set pointer offset
                               (bitwise-copy-bit-field
                                val bit-offset end-offset
                                (bitwise-arithmetic-shift-left
                                 (bitwise-and val mask)
                                 bit-offset)))))))
               (else
                (lambda (pointer val) (ptr-set pointer offset val))))))))

  ;;@defspec let*-pointers (binding ...) body ...
  ;;
  ;; Establish pointer bindings for callouts. Each @var{binding} can
  ;; be one of the following forms:
  ;;
  ;; @table @samp
  ;;
  ;; @item ((<id> <= <bv-expr> [<start> [<end>]]))
  ;;
  ;; Binds @var{<id>} to a pointer into @var{<bv-expr>}, which must
  ;; evaluate to a bytevector. The memory area pointed to by
  ;; @var{<id>} starts at offset @var{<start>} within the bytevecor and ends
  ;; at @var{<end>}. @var{<start>} and @var{<end>} default to 0 and the length
  ;; of the bytevector, respectively.
  ;;
  ;; @item ((<id> => <bv-expr> [<start> [<end>]]))
  ;;
  ;; @item ((<id> <cleanup-proc> <ptr-expr>))
  ;; @end table
  ;;
  ;;@end defspec
  (define-syntax let*-pointers
    (lambda (stx)
      (define (process-bindings bindings)
        (loop continue ((with bds '())
                        (with cleanup-actions '())
                        (for binding (in-list bindings)))
          => (values (reverse bds)
                     cleanup-actions)
          (define (frozen-bytes direction id expr args)
            (with-syntax (((fbytes) (generate-temporaries '(frozen-bytes))))
              (continue (=> bds
                            (append-reverse
                             (list #`(fbytes
                                      (freeze-bytes '#,direction #,expr #,@args))
                                   #`(#,id (frozen-bytes-pointer fbytes)))
                             bds))
                        (=> cleanup-actions
                            (cons #'(unfreeze-bytes fbytes)
                                  cleanup-actions)))))
          (syntax-case binding (=> <=)
            ;; input data
            ((id <= expr arg ...)
             (frozen-bytes #'in #'id #'expr #'(arg ...)))
            ((id => expr arg ...)
             (frozen-bytes #'out #'id #'expr #'(arg ...)))
            ((id free expr)
             (continue (=> bds (cons #'(id expr) bds))
                       (=> cleanup-actions
                           (cons #'(free expr) cleanup-actions)))))))
      (syntax-case stx ()
        ((_ (binding ...) body0 body ...)
         (receive (bds cleanup-actions)
                  (process-bindings #'(binding ...))
           #`(let* #,bds
               (receive results (begin body0 body ...)
                 #,@cleanup-actions
                 (apply values results))))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
