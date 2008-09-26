; arch-tag: 244d45c2-888e-4312-bf52-99522893a38d

;;@ Call @2 with @code{(current-input-port)} referring to @0.
(define (with-input-from-port port thunk)
  (with-current-ports port (current-output-port) (current-error-port)
                      thunk))

;;@ Call @2 with @code{(current-output-port)} referring to @0.
(define (with-output-to-port port thunk)
  (with-current-ports (current-input-port) port (current-error-port)
                      thunk))

