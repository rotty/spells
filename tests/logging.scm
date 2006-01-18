;; delimited-readers.scm -- unit tests for delimited-readers.scm
;; arch-tag: 9eded783-24d0-4fac-8044-b1bd464815de

;; Copyright (C) 2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Wed Jan 18 12:32:10 CET 2006

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(define (expand-handlers note-taker config)
  (map (lambda (entry)
         (case (car entry)
           ((handlers) (cons 'handlers
                             (map (lambda (params)
                                    (apply make-log-handler
                                           (note-taker (car params)) (cdr params)))
                                  (cdr entry))))
           (else entry)))
       config))

(define (tester configs procs)
  (let ((passes '())
        (failures '())
        (obj (list 'test-object)))
    (define (note-taker name)
      (lambda (entry)
        (cond ((and  (log-entry? entry)
                     (eq? (log-entry-object entry) obj))
               (set! passes (cons name passes)))
              (else
               (set! failures (cons name failures))))))
    
    (for-each (lambda (entry)
                (configure-logger (car entry)
                                  (expand-handlers note-taker (cdr entry))))
              configs)
    
    (map (lambda (proc)
           (set! passes '())
           (set! failures '())
           (proc obj)
           (if (not (null? failures))
               (cons 'failed (reverse failures))
               (cons 'passed (reverse passes))))
         procs)))

(testeez "Testing logging library"
  (test/equal "handler invocation"
    (tester '((() (handlers (root)))) (list (make-log '() 'info)))
    '((passed root)))
  (test/equal "propagation"
    (tester '((() (handlers (root)))
              ((test) (handlers (test))))
            (list (make-log '() 'info)
                  (make-log '(test) 'info)))
    '((passed root) (passed test root)))
  (test/equal "threshold"
    (tester '((() (handlers (root error)))
              ((test) (handlers (test))))
            (list (make-log '(test) 'info)
                  (make-log '(test) 'error)))
    '((passed test) (passed test root))))
