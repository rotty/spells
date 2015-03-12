#!r6rs
;;; logging.scm --- Unit tests for (spells logging)

;; Copyright (C) 2009, 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (spells logging)
        (wak trc-testing))

;;; Utility procedures

(define (expand-handlers note-taker config)
  (map (lambda (entry)
         (case (car entry)
           ((handlers) (cons 'handlers
                             (map (lambda (params)
                                    (append (cdr params)
                                            (list (note-taker (car params))) ))
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
                (set-logger-properties! (car entry)
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

(define test-logger (make-logger root-logger 'test))

;;; Test suite

(define-test-suite logging-tests
  "Logging library")

(define-test-case logging-tests handler-invocation ()
  (test-equal '((passed root))
    (tester `((,root-logger (handlers (root))))
            (list (make-log root-logger 'info)))))

(define-test-case logging-tests propagation ()
  (test-equal
      '((passed root) (passed test root))
    (tester `((,root-logger (handlers (root)))
              (,test-logger (handlers (test))))
              (list (make-log root-logger 'info)
                    (make-log test-logger 'info)))))

(define-test-case logging-tests threshold ()
  (test-equal
      '((passed test) (passed test root))
    (tester `((,root-logger (handlers (root error)))
              (,test-logger (handlers (test))))
              (list (make-log test-logger 'info)
                    (make-log test-logger 'error)))))

(run-test-suite logging-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
