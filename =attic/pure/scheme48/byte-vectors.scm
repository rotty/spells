(define make-u8vector make-byte-vector)
(define u8vector byte-vector)
(define u8vector? byte-vector?)
(define u8vector-length byte-vector-length)
(define u8vector-ref byte-vector-ref)
(define u8vector-set! byte-vector-set!)

(define make-u8vector make-byte-vector)
(define u8vector byte-vector)
(define u8vector? byte-vector?)
(define u8vector-length byte-vector-length)
(define u8vector-ref byte-vector-ref)
(define u8vector-set! byte-vector-set!)

(define (u8vector->list vec)
  (let loop ((i (- (u8vector-length vec) 1))
             (result '()))
    (if (< i 0)
        result
        (loop (- i 1) (cons (u8vector-ref vec i) result)))))

(define (list->u8vector lst)
  (apply u8vector lst))

;; arch-tag: c50ab679-3546-4b9c-a383-6d2758ee7ef9
