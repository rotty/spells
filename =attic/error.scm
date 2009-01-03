;;@ The argument @1 should be a string. The procedure error will
;; signal an error, as described in R5RS, and it will report the
;; message @1 and the objects @2. Signalling an error means raising
;; a condition of type @ref{spells.condition &error} or a subtype.
(define (error reason . args)
  (raise (condition (&error)
                    (&message (message reason))
                    (&irritants (values args)))))

;;@ Used to indicate a error that was caused by an invalid function
;; call, e.g. argument type error.
(define (call-error reason proc . args)
  (apply error reason proc args))

(define (syntax-error reason form . irritants)
  (apply error reason form irritants))

;; arch-tag: dc950326-f033-4380-bc8b-31b98545a5c3
