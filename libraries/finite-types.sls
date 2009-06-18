;;; finite-types.sls --- Scheme48-style finite types

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Based on code from Scheme48 1.8, Copyright (c) 1993-2008 by Richard
;; Kelsey and Jonathan Rees.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells finite-types)
  (export define-finite-type
          finite-type-case)
  (import (rnrs base)
          (rnrs syntax-case)
          (srfi :9 records))

  (define-syntax define-finite-type
    (lambda (stx)
      (syntax-case stx ()
        ((_ dispatcher rtd
            instance-fields
            predicate
            instance-vector
            name-accessor
            index-accessor
            (field-tag . field-get&set) ...
            ((instance-name . instance-field-values) ...))
         (with-syntax (((constructor-invocation ...)
                        (let loop ((invocations '())
                                   (i 0)
                                   (names #'(instance-name ...))
                                   (values #'(instance-field-values ...)))
                          (if (null? names)
                              (reverse invocations)
                              (loop (cons #`(constructor '#,(car names)
                                                         #,i
                                                         #,@(car values))
                                          invocations)
                                    (+ i 1)
                                    (cdr names)
                                    (cdr values))))))
           #'(begin
               (define-record-type rtd
                 (constructor name index . instance-fields)
                 predicate
                 (name name-accessor)
                 (index index-accessor)
                 (field-tag . field-get&set) ...)
               (define instance-vector
                 (vector constructor-invocation ...))
               (define-dispatch dispatcher (instance-name ...) instance-vector)))))))

  (define-syntax define-dispatch
    (lambda (stx)
      (syntax-case stx ()
        ((_ name (instance-name ...) instance-vector)
         #`(define-syntax name
             (lambda (stx)
               (syntax-case stx ()
                 ((dispatcher-name tag)
                  (let loop ((names (syntax->datum #'(instance-name ...)))
                             (i 0))
                    (cond ((null? names)
                           (syntax-violation (syntax->datum #'dispatcher-name)
                                             "no such instance"
                                             (syntax->datum #'tag)))
                          ((eq? (syntax->datum #'tag) (car names))
                           #`(vector-ref instance-vector #,i))
                          (else
                           (loop (cdr names) (+ i 1)))))))))))))


  (define-syntax finite-type-case
    (syntax-rules (else)
      ((_ dispatcher value-expr ((name ...) expr ...) ... (else alt-expr ...))
       (let ((val value-expr))
         (cond ((or (eq? (dispatcher name) val) ...)
                expr ...) ...
               (else
                alt-expr ...))))
      ((_ dispatcher value-expr ((name ...) expr ...) ...)
       (let ((val value-expr))
         (cond ((or (eq? (dispatcher name) val) ...)
                expr ...) ...)))))

)
