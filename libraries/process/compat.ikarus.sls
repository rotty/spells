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

          run-shell-command)
  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (rnrs io ports)
          (prefix (only (ikarus)
                        process*
                        waitpid
                        wstatus-exit-status
                        wstatus-received-signal
                        system)
                  ik:)
          (srfi :8 receive)
          (srfi :9 records)
          (spells tracing)
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

  (define (spawn-process env stdin stdout stderr prog . args)
    (receive (pid p-in p-out p-err)
             (apply ik:process* #f env stdin stdout stderr
                    (x->strlist (cons prog args)))
      (make-process pid p-in p-out p-err)))

  (define (wstatus->values wstatus)
    (values (ik:wstatus-exit-status wstatus)
            (ik:wstatus-received-signal wstatus)))
  
  (define (wait-for-process process)
    (wstatus->values (ik:waitpid (process-pid process))))

  (define (run-shell-command cmd)
    ;; This is a hack, but works (at least) on Linux. See
    ;; <https://bugs.launchpad.net/ikarus/+bug/349210>.
    (let* ((wstatus (ik:system cmd))
           (sig (bitwise-and wstatus #xff)))
      (values (bitwise-arithmetic-shift-right wstatus 8)
              (if (= sig 0) #f sig)))))

;; Local Variables:
;; scheme-indent-styles: ((ik:register-callback 1))
;; End:
