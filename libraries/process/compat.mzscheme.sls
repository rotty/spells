;;; compat.mzscheme.sls --- OS process, mzscheme compat.

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
          call-with-process-output

          run-shell-command)
  (import (rnrs base)
          (rnrs io ports)
          (only (scheme base)
                subprocess subprocess-wait subprocess-status)
          (only (scheme system)
                system/exit-code)
          (srfi :8 receive)
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
    (receive (process stdout-port stdin-port stderr-port)
        (apply subprocess #f #f #f (x->strlist (cons prog args)))
      (make-process process stdin-port stdout-port stderr-port)))

  (define (wait-for-process process)
    (subprocess-wait (process-pid process))
    (values (subprocess-status (process-pid process)) #f))

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
          (apply values (append status+signal results))))))

  (define (run-shell-command cmd)
    (let ((retval (system/exit-code cmd)))
      (if (>= 0 retval)
          (values retval #f)
          (values retval 'unknown))))

  )
