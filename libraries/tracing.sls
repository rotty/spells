#!r6rs

;;@ Trace procedures for debugging.
(library (spells tracing)
  (export trace-define trace-lambda trace-procedure)
  (import (rnrs base)
          (spells tracing compat))

  (define-syntax trace-procedure
    (syntax-rules ()
      ((trace-procedure <label> <proc-expr>)
       (let ((proc <proc-expr>))
         (trace-lambda <label> args
           (apply proc args))))))

  )
