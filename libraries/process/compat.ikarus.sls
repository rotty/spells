;;; compat.ikarus.sls --- OS processes, Ikarus compat.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
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
                        process-nonblocking
                        waitpid
                        wstatus-exit-status
                        wstatus-received-signal)
                  ik:)
          (srfi :8 receive)
          (srfi :9 records)
          (spells ports)
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
                  (error 'x->strlist
                         "cannot coerce to string list"
                         lst))))
         lst))

  (define (spawn-process env prog . args)
    (receive (pid stdin-port stdout-port stderr-port)
        (apply ik:process-nonblocking (x->strlist (cons prog args)))
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
    (let ((p (apply spawn-process env prog args))
          (stdout (standard-output-port))
          (stderr (standard-error-port)))
      (copy-port (process-errors p) stderr)
      (flush-output-port stderr)
      (copy-port (process-output p) stdout)
      (flush-output-port stdout)
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
