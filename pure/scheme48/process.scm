;; process.scm -*- scheme48-package: spells.process -*-
;; Copyright (C) 2005 by Free Software Foundation, Inc.

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

(define (spawn-process prog . args)
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
             (apply exec prog args))))))

(define (wait-for-process process)
  (wait-for-child-process (process-pid process))
  (values (process-id-exit-status (process-pid process))
          (process-id-terminating-signal (process-pid process))))

(define (close-process-ports process)
  (close-input-port (process-output process))
  (close-output-port (process-input process))
  (close-input-port (process-errors process)))

(define (run-process prog . args)
  (let ((id (fork)))
    (cond (id
           (wait-for-child-process id)
           (values (process-id-exit-status id)
                   (process-id-terminating-signal id)))
          (else
           (apply exec prog args)))))

(define (run-process/string prog . args)
  (let* ((process (apply spawn-process prog args))
         (output (process-output process))
         (result (string-unfold eof-object?
                                values
                                (lambda (seed)
                                  (read-char output))
                                (read-char output))))
    (close-process-ports process)
     (receive (status signal) (wait-for-process process)
       (values status signal result))))

;;; process.scm ends here
