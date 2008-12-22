#!r6rs

(library (spells tracing compat)
  (export trace-lambda trace-define)
  (import (rnrs base)
          (rnrs io simple))

  (define-syntax trace-define
    (syntax-rules ()
      ((trace-define (name arg ...) body ...)
       (define (name arg ...) body ...))))

  (define-syntax trace-lambda
    (syntax-rules ()
      ((trace-lambda name args body ...)
       (lambda args body ...)))))
