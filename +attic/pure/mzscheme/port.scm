(#%require (only mzscheme parameterize))

(define (with-current-ports in out error thunk)
  (parameterize ((current-input-port in)
                 (current-output-port out)
                 (current-error-port error))
    (thunk)))

(define force-output flush-output)

(define-syntax file-options
  (syntax-rules ()
    ((_ name ...) '(name ...))))

(define (file-optlist->optsym options)
  (define (opt-set? opt) (memq opt options))
  (cond 
   ((and (opt-set? 'create) (opt-set? 'exclusive))      'error)
   ((opt-set? 'truncate)                                'truncate)
   ((opt-set? 'append)                                  'append)
   (else                                                'update)))

(define (open-output-file/options pathname options)
  (open-output-file pathname 'binary (file-optlist->optsym options)))

;; arch-tag: 6c4aa866-7f16-4704-83a4-3f3944783e22
