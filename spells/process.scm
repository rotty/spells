;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated). The return values are
;; the exit status, terminating signal (if the process has been
;; terminated by a signal) and the standard output captured in a
;; string, list of strings or s-expressions, respectively.
;;
;; Using @code{srfi-8}, @0 can be used as follows:
;; @lisp
;; (receive (status signal output) (run-process/string #f "cat /etc/hostname")
;;   ...)
;; @end lisp
(define (run-process/string env prog . args)
  (let* ((process (apply spawn-process env prog args))
         (output (transcoded-port (process-output process) (native-transcoder)))
         (result (string-unfold eof-object?
                                values
                                (lambda (seed)
                                  (read-char output))
                                (read-char output))))
    (close-process-ports process)
    (receive (status signal) (wait-for-process process)
      (values status signal result))))

(define (run-process/lines env prog . args)
  (call-with-process-output env (cons prog args)
    port->lines))

(define (run-process/sexps env prog . args)
  (call-with-process-output env (cons prog args)
    port->sexps))

;;@end
(define (port->lines port)
  (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))

(define (port->sexps port)
  (unfold eof-object? values (lambda (seed) (read port)) (read port)))

