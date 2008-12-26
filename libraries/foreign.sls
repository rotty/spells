#!r6rs

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

          make-c-callout make-c-callback

          malloc free memcpy memset

          dlopen dlsym dlclose dlerror

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
          (spells foreign compat)
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

  (define pointer-int8-ref   (make-pointer-c-getter 'int8))
  (define pointer-int8-set!  (make-pointer-c-setter 'int8))
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
                (lambda (pointer val) (ptr-set pointer offset val)))))))))
