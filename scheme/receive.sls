#!r6rs
(library (spells receive)
  (export receive)
  (import (rnrs base))

  (define-syntax receive
    (syntax-rules ()
      ((receive formals expression body ...)
       (call-with-values (lambda () expression)
         (lambda formals body ...))))))
