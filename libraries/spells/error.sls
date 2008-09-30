#!r6rs
(library (spells error)
  (export error error-who)
  (import 
    (rename (rnrs base) (error rnrs:error))
    (spells parameter))
  
  (define error-who (make-parameter #f))
  
  (define (error . args)
    (apply rnrs:error (error-who) args))
  )
