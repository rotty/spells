;; Define byte-vectors in terms of SRFI-4 (or SRFI-66)
(define make-byte-vector make-u8vector)
(define byte-vector u8vector)
(define byte-vector? u8vector?)
(define byte-vector-length u8vector-length)
(define byte-vector-ref u8vector-ref)
(define byte-vector-set! u8vector-set!)

;; arch-tag: 454298e6-206b-4d10-8f8d-73831b14717a
