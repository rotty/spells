(library (spells foreign)
  (export make-pointer-ref
          pointer-ref-pointer pointer-set-pointer
          pointer-set-char pointer-ref-unsigned-char
          pointer->integer integer->pointer
          make-callout
          malloc free memcpy
          dlopen dlsym dlclose
          c-type-sizeof c-type-alignof c-type-align
          c-compound-element-fetcher)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (rnrs lists)
          (rnrs bytevectors)
          (spells alist)
          (spells foreign config)
          (ikarus foreign))

  (define type-aliases
    (map (lambda (ctype)
           (let ((signed? (memq ctype '(char short int long llong))))
             (cons
              (case (c-type-sizeof ctype)
                ((1)  (if signed? 'int8 'uint8))
                ((2)  (if signed? 'int16 'uint16))
                ((4)  (if signed? 'int32 'uint32))
                ((8)  (if signed? 'int64 'uint64))
                (else
                 (assertion-violation 'type-aliases
                                      "unexpected return value from c-type-sizeof"
                                      ctype)))
              ctype)))
         '(char uchar short ushort int uint long ulong llong ullong)))

  (define (c-type-align ctype n)
    (let ((alignment (c-type-alignof ctype)))
      (+ n (mod (- alignment (mod n alignment)) alignment))))
  
  (define (c-compound-element-fetcher type offset bit-offset bits)
    (case type
      ((record union array)
         (lambda (pointer)
           (integer->pointer (+ (pointer->integer pointer) offset))))
      (else
       (let ((ptr-ref (make-pointer-ref type)))
         (cond ((and bits bit-offset)
                (let ((end-offset (+ bit-offset bits)))
                  (lambda (pointer)
                    (let ((val (ptr-ref pointer offset)))
                      (bitwise-bit-field val bit-offset end-offset)))))
               (else
                (lambda (pointer) (ptr-ref pointer offset))))))))

  (define (make-pointer-ref sym)
    (define (primitive-ref sym)
      (case sym
        ((char)   pointer-ref-signed-char)
        ((uchar)  pointer-ref-unsigned-char)
        ((short)  pointer-ref-signed-short)
        ((ushort) pointer-ref-unsigned-short)
        ((int)    pointer-ref-signed-int)
        ((uint)   pointer-ref-unsigned-int)
        ((long)   pointer-ref-signed-long)
        ((ulong)  pointer-ref-unsigned-long)
        ((float)  pointer-ref-float)
        ((double) pointer-ref-double)
        ((pointer) pointer-ref-pointer)
        (else #f)))
    (or (primitive-ref sym)
         (let ((alias (assq-ref type-aliases sym)))
           (or (and alias (primitive-ref alias))
               (error 'make-pointer-ref "invalid type" sym)))))

  (define memcpy
    (lambda (p1 p2 n)
      (cond ((and (pointer? p1) (bytevector? p2))
             (do ((i 0 (+ i 1)))
                 ((>= i n))
               (pointer-set-char p1 i (bytevector-u8-ref p2 i))))
            ((and (bytevector? p1) (pointer? p2))
             (do ((i 0 (+ i 1)))
                 ((>= i n))
               (bytevector-u8-set! p1 i (pointer-ref-unsigned-char p2 i))))
            (else
             (error 'memcpy "need pointer and bytevector" p1 p2)))
      p1)))
