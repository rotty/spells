;; process.scm -*- scheme48-package: spells.process -*-
;; Copyright (C) 2005-2006 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Wed Nov 09, 2005 02:33

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Library Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Library Public License for more details.
;;
;; You should have received a copy of the GNU Library Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Comentary:

;; Helper functions for creating procedures that lauch system
;; processes.

;;; Code:

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

(define (env->strlist env)
  (and (list? env)
       (map (lambda (e) (string-append (car e) "=" (cdr e))) env)))

(define (spawn-process env prog . args)
  (let-values (((in-in in-out) (open-pipe))
               ((out-in out-out) (open-pipe))
               ((err-in err-out) (open-pipe)))
    (let ((id (fork)))
      (cond (id
             (close-input-port in-in)
             (close-output-port out-out)
             (close-output-port err-out)
             (make-process id in-out out-in err-in))
            (else
             (remap-file-descriptors! in-in out-out err-out)
             (exec-with-alias prog #t (env->strlist env) (x->strlist (cons prog args))))))))

(define (wait-for-process process)
  (wait-for-child-process (process-pid process))
  (values (process-id-exit-status (process-pid process))
          (process-id-terminating-signal (process-pid process))))

(define (close-process-ports process)
  (let ((input (process-input process))
        (output (process-output process))
        (errors (process-errors process)))
    (if input (close-output-port input))
    (if output (close-input-port output))
    (if errors (close-input-port errors))))

(define (run-process env prog . args)
  (let ((id (fork)))
    (cond (id
           (wait-for-child-process id)
           (values (process-id-exit-status id)
                   (process-id-terminating-signal id)))
          (else
           (if env
               (apply exec-with-environment
                      (cons prog (cons (env->strlist env) args)))
               (exec prog args))))))

(define (run-process/string env prog . args)
  (let* ((process (apply spawn-process (cons env (x->strlist (cons prog args)))))
         (output (process-output process))
         (result (string-unfold eof-object?
                                values
                                (lambda (seed)
                                  (read-char output))
                                (read-char output))))
    (close-process-ports process)
    (receive (status signal) (wait-for-process process)
      (values status signal result))))

(define (open-process-input env prog . args)
  (receive (in-in in-out) (open-pipe)
    (let ((id (fork)))
      (cond (id
             (close-input-port in-in)
             (make-process id in-out #f #f))
            (else
             (close-output-port in-out)
             (remap-file-descriptors! in-in (current-output-port) (current-error-port))
             (exec-with-alias prog #t (env->strlist env) (x->strlist (cons prog args))))))))

(define (open-process-output env prog . args)
  (receive (out-in out-out) (open-pipe)
    (let ((id (fork)))
      (cond (id
             (close-output-port out-out)
             (make-process id #f out-in #f))
            (else
             (close-input-port out-in)
             (remap-file-descriptors! (current-input-port) out-out (current-error-port))
             (exec-with-alias prog #t (env->strlist env) (x->strlist (cons prog args))))))))

(define (call-with-process-input env prog+args receiver)
  (let* ((process (apply open-process-input env prog+args))
         (port (process-input process)))
    (receiver port)
    (close-process-ports process)
    (wait-for-process process)))

(define (call-with-process-output env prog+args receiver)
  (let* ((process (apply open-process-output env prog+args))
         (port (process-output process)))
    (receive results (receiver port)
      (close-process-ports process)
      (receive status+signal (wait-for-process process)
        (apply values (append status+signal results))))))

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

;;; process.scm ends here
