;;; compat.ypsilon.sls --- process compat library for Ypsilon

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

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
          (spells receive)
          (spells record-types)
          (spells pathname)
          (prefix (only (core primitives)
                        process
                        process-wait)
                  yp:)
          (only (core destructuring) destructuring-bind))

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
    (destructuring-bind (pid stdin stdout stderr) (apply yp:process prog args)
      (make-process pid stdin stdout stderr)))
  
  (define (wait-for-process process)
    (values (yp:process-wait (process-pid process) #t)
            #f))
  
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
  
)
