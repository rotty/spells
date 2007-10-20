(scmxlate-include "byte-vector-aliases.scm")
(scmxlate-include "srfi-66-ops.scm")

;; Gauche's module system doesn't allow re-export :/

(define make-u8vector make-u8vector)
(define u8vector u8vector)
(define u8vector? u8vector?)
(define u8vector-length u8vector-length)
(define u8vector-ref u8vector-ref)
(define u8vector-set! u8vector-set!)
(define u8vector->list u8vector->list)
(define list->u8vector list->u8vector)
(define u8vector=? u8vector=?)
(define u8vector-compare u8vector-compare)
(define u8vector-copy! u8vector-copy!)
(define u8vector-copy u8vector-copy)

;; arch-tag: 39e3ce99-fa3c-4ba0-bfe1-a88d7dbb6fbf
