;;; process.scm --- OS process interface.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define (close-process-ports process)
  (let ((input (process-input process))
        (output (process-output process))
        (errors (process-errors process)))
    (if input (close-port input))
    (if output (close-port output))
    (if errors (close-port errors))))

;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated, yielding the the exit
;; status, and the terminating signal (if the process has been
;; terminated by a signal, or false otherwise).
(define (run-process env prog . args)
  (wait-for-process 
   (apply spawn-process
          env
          (standard-input-port)
          (standard-output-port)
          (standard-error-port)
          prog args)))

;;@ Run @2, which must be a list of the executable name and any
;; arguments with the environment @1.
;;
;; The procedure of one argument @3 is passed an output port which
;; corresponds to the standard input of the process. @3 and the
;; process execute at the same time; when @3 returns, @0 waits for the
;; process to terminate and returns the exit status, terminating
;; signal (if the process has been terminated by a signal, or false
;; otherwise) and the values returned by @3.
(define (call-with-process-input env prog+args receiver)
  (let* ((process (apply spawn-process
                         env
                         #f
                         (standard-output-port)
                         (standard-error-port)
                         prog+args))
         (port (transcoded-port (process-input process) (native-transcoder))))
    (receiver port)
    (close-process-ports process)
    (wait-for-process process)))

;;@ Run @2, which must be a list of the executable name and any
;; arguments with the environment @1.
;;
;; The procedure of one argument @3 is passed an input port which
;; corresponds to the standard output of the process. @3 and the
;; process execute at the same time; when @3 returns, @0 waits for the
;; process to terminate and returns the exit status, terminating
;; signal (if the process has been terminated by a signal, or false
;; otherwise) and the values returned by @3.
;;
;; This means, given a procedure @code{port->lines},
;; @ref{spells.process run-process/lines,run-process/lines} can be
;; implemented like this:
;; @lisp
;; (define (run-process/lines env prog . args)
;;   (call-with-process-output env (cons prog args)
;;     port->lines))
;; @end lisp
(define (call-with-process-output env prog+args receiver)
  (let* ((process (apply spawn-process
                         env
                         (standard-input-port)
                         #f
                         (standard-error-port)
                         prog+args))
         (port (transcoded-port (process-output process) (native-transcoder))))
    (receive results (receiver port)
      (close-process-ports process)
      (receive status+signal (wait-for-process process)
        (apply values (append status+signal results))))))

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
  (call-with-process-output env (cons prog args)
    (lambda (output)
      (string-unfold eof-object?
                     values
                     (lambda (seed)
                       (read-char output))
                     (read-char output)))))

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

