#!r6rs

;;@ @uref{http://srfi.schemers.org/srfi-23/srfi-23.html, SRFI 23}
;;  compatability library.
(library (spells error)
  (export error error-who make-error-signaller)
  (import 
    (rename (rnrs base) (error rnrs:error))
    (spells parameter))
  
  (define error-who (make-parameter #f))
  
  (define (error . args)
    (apply rnrs:error (error-who) args))

  (define (make-error-signaller who)
    (lambda args
      (apply rnrs:error who args))))
