(library (spells foreign)
  (export make-pointer-ref
          pointer->integer integer->pointer
          malloc free memcpy
          c-type-sizeof c-type-alignof)
  (import (rnrs base)
          (rnrs control)
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
  
  (define (make-pointer-ref sym)
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
      (else
       (let ((alias (assq-ref type-aliases sym)))
         (or (and alias (make-pointer-ref alias)) 
             (error 'make-pointer-ref "invalid type" sym))))))

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
