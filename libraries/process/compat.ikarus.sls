#!r6rs

(library (spells process compat)
  (export process?
          process-pid
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process
          close-process-ports
          run-process
          call-with-process-input
          call-with-process-output)
  (import (rnrs base)
          (rnrs io ports)
          (prefix (only (ikarus)
                        process
                        waitpid
                        wstatus-exit-status
                        wstatus-received-signal)
                  ik:)
          (spells receive)
          (spells record-types)
          (spells pathname))

  (define-record-type process
    (make-process pid input output errors)
    process?
    (pid process-pid)
    (input process-input)
    (output process-output)
    (errors process-errors))

  (define (x->strlist lst)
    (map (lambda (s)
           (cond ((string? s)   s)
                 ((pathname? s) (x->namestring s))
                 (else
                  (error "cannot coerce to string list" lst))))
         lst))

  (define (spawn-process env prog . args)
    (receive (pid stdin-port stdout-port stderr-port)
        (apply ik:process (x->strlist (cons prog args)))
      (make-process pid stdin-port stdout-port stderr-port)))

  (define (wait-for-process process)
    (let ((wstatus (ik:waitpid (process-pid process))))
      (values (ik:wstatus-exit-status wstatus)
              (ik:wstatus-received-signal wstatus))))

  (define (close-process-ports process)
    (let ((input (process-input process))
          (output (process-output process))
          (errors (process-errors process)))
      (if input (close-port input))
      (if output (close-port output))
      (if errors (close-port errors))))

  (define (run-process env prog . args)
    (let ((p (apply spawn-process env prog args)))
      (close-process-ports p)
      (wait-for-process p)))

  (define (call-with-process-input env prog+args receiver)
    (let* ((process (apply spawn-process env prog+args))
           (port (transcoded-port (process-input process) (native-transcoder))))
      (receiver port)
      (close-process-ports process)
      (wait-for-process process)))

  (define (call-with-process-output env prog+args receiver)
    (let* ((process (apply spawn-process env prog+args))
           (port (transcoded-port (process-output process) (native-transcoder))))
      (receive results (receiver port)
        (close-process-ports process)
        (receive status+signal (wait-for-process process)
          (apply values (append status+signal results)))))))
