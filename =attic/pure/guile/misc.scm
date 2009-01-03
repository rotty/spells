
(define sleep-seconds sleep)
(define (eof-object) the-eof-object)
(define (compose f g) (lambda args (f (apply g args))))
(define (unspecific) (if #f #f))
(define (scheme-dialect) 'guile)
