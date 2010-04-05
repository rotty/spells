#!r6rs

(import (rnrs)
        (rnrs r5rs)
        (only (srfi :13) string-concatenate)
        (srfi :64 testing)
        (spells irregex)
        (spells format)
        (spells include)
        (spells pathname)
        (spells testing run-env)
        (spells delimited-readers)
        (spells string-utils)
        (spells match))

(define-syntax test
  (syntax-rules ()
    ((test expected expr)
     (test-equal expected expr))
    ((test name expected expr)
     (test-equal name expected expr))))

(define call-with-output-string call-with-string-output-port)

(define (warning message . irritants)
  (raise-continuable (make-warning)
                     (make-message-condition message)
                     (make-irritants-condition irritants)))

(define (port-for-each proc read-proc)
  (let loop ()
    (let ((object (read-proc)))
      (unless (eof-object? object)
        (proc object)
        (loop)))))

(define (sprintf fmt . args)
  (apply format #f fmt args))

(include-file ((spells private) test-irregex))
