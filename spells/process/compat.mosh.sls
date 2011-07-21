#!r6rs
; compat.mosh.sls - (spells process) compatibility layer for Mosh
; copyright: 2011 David Banks <amoebae@gmail.com>; license: BSD-3-clause

(library (spells process compat)
  (export process?
          process-id
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process

          get-process-id
          run-shell-command)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (only (srfi :1) first second)
          (srfi :9)
          (prefix (mosh process) mosh:)
          (spells pathname))

  (define-record-type process
    (make-process pid input output errors)
    process?
    (pid process-id)
    (input process-input)
    (output process-output)
    (errors process-errors))

  (define (->str who s)
    (cond
     ((string? s)   s)
     ((pathname? s) (->namestring s))
     (else
      (assertion-violation who "cannot coerce to string" s))))

  ; Used by SPAWN-PROCESS.
  (define (maybe-create-pipe val)
    (if val
        #f
        (call-with-values mosh:pipe list)))

  (define pipe:in first)
  (define pipe:out second)

  ; The structure returned by spawn-process has the following members:
  ; * An output port representing stdin of the subprocess.
  ; * An input port represting stdout of the subprocess.
  ; * An input port representing stderr of the subprocess.
  (define (spawn-process env stdin stdout stderr prog . args)
    (let ((convert (lambda (x) (->str 'spawn-process x)))
          (in-ports (maybe-create-pipe stdin))
          (out-ports (maybe-create-pipe stdout))
          (err-ports (maybe-create-pipe stderr)))
      (let ((io-list (list (or stdin (pipe:in in-ports))
                           (or stdout (pipe:out out-ports))
                           (or stderr (pipe:out err-ports)))))
        (let-values (((pid in out err) (apply mosh:spawn
                                              (convert prog)
                                              (map convert args)
                                              io-list
                                              #f                ; no PATH search
                                              (if env env '())))) 
          (make-process pid
                        (if stdin #f (pipe:out in-ports))
                        (if stdout #f (pipe:in out-ports))
                        (if stderr #f (pipe:in err-ports)))))))

  
  (define (wait-for-process process)
    (let-values (((child-pid status termsig) (mosh:waitpid (process-id process))))
      (values status termsig)))

  (define (get-process-id)
    (mosh:getpid))

  (define (run-shell-command cmd)
    (let-values (((stdout exit termsig) (mosh:call-process cmd)))
      (values exit termsig)))
)

  
