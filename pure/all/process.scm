(define (run-process/string env prog . args)
  (let* ((process (apply spawn-process env prog args))
         (output (process-output process))
         (result (string-unfold eof-object?
                                values
                                (lambda (seed)
                                  (read-char output))
                                (read-char output))))
    (close-process-ports process)
    (receive (status signal) (wait-for-process process)
      (values status signal result))))

(define (port->lines port)
  (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))

(define (port->sexps port)
  (unfold eof-object? values (lambda (seed) (read port)) (read port)))

(define (run-process/lines env prog . args)
  (call-with-process-output env (cons prog args)
    port->lines))

(define (run-process/sexps env prog . args)
  (call-with-process-output env (cons prog args)
    port->sexps))
