(scmxlate-ignore-define with-input-from-port with-output-to-port current-error-port)

(define (with-current-ports in out err thunk)
  (with-input-from-port in
    (lambda ()
      (with-output-to-port out
        (lambda ()
          (with-error-to-port err (thunk)))))))

(define force-output flush)

(define-syntax file-options
  (syntax-rules ()
    ((_ name ...) '(name ...))))

;; arch-tag: 697a7950-414e-4b5a-964f-576758bc585b
