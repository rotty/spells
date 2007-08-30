(define (with-current-ports in out error thunk)
  (let ((orig-in (current-input-port))
        (orig-out (current-output-port))
        (orig-err (current-error-port)))
    (dynamic-wind
        (lambda ()
          (set-current-input-port in)
          (set-current-output-port out)
          (set-current-error-port err))
        (lambda ()
          (set-current-input-port orig-in)
          (set-current-output-port orig-out)
          (set-current-error-port orig-err))
        thunk)))

(define-syntax file-options
  (syntax-rules ()
    ((_ name ...) '(name ...))))

(define (file-optlist->mode optlist)
  (apply logior (map (lambda (opt)
                       (case opt
                         ((create) O_CREAT)
                         ((exclusive) O_EXCL)
                         ((truncate) O_TRUNC)
                         ((append) O_APPEND)
                         (else (error "unknown file option" opt))))
                     optlist)))

(define (open-output-file/options pathname options)
  (open pathname 'binary (file-optlist->mode options)))

;; arch-tag: 6c4aa866-7f16-4704-83a4-3f3944783e22
