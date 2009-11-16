;;; operations.scm --- T-like operations.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; A generic dispatch system similiar to operations in T

;;; Code:
#!r6rs

;;@ A simple object system.
(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (rnrs lists)
          (only (srfi :1 lists) any)
          (spells procedure-annotations))
  
;; Auxiliary syntax
(define-syntax %method-clauses->handler
  (syntax-rules ()
    ((%method-clauses->handler ((?op . ?params) ?body ...) ...)
     (let ((methods (list (cons ?op (lambda ?params ?body ...)) ...)))
       (lambda (op)
         (cond ((assq op methods) => cdr)
               (else #f)))))))

(define-syntax object
  (syntax-rules ()
    ((object ?proc ?method-clause ...)
     (make-object ?proc (%method-clauses->handler ?method-clause ...)))))

(define (make-object proc handler)
  (annotate-procedure
   (or proc (lambda args (error 'make-object "object is not applicable"))) handler))

(define-syntax operation
  (syntax-rules ()
    ((operation "%named" ?name ?default ?method-clause ...)
     (make-operation '?name ?default (%method-clauses->handler ?method-clause ...)))
    ((operation ?default ?method-clause ...)
     (operation "%named" #f ?default ?method-clause ...))))

(define (make-operation name default handler)
  (letrec ((op (make-object
                (lambda (obj . args)
                  (cond ((and (procedure? obj) ((procedure-annotation obj) op))
                         => (lambda (method)
                              (apply method obj args)))
                        (default
                          (apply default obj args))
                        (else
                         (error 'operation
                                "operation is not available"
                                obj
                                (or name op)))))
                handler)))
    op))

(define-syntax define-operation
  (syntax-rules ()
    ((define-operation (?name . ?args))
     (define ?name (operation "%named" ?name #f)))
    ((define-operation (?name . ?args) ?body1 ?body ...)
     (define ?name (operation "%named" ?name (lambda ?args ?body1 ?body ...))))))

(define (join object1 . objects)
  (make-object object1
               (lambda (op)
                 (let ((method (any (lambda (o) ((procedure-annotation o) op))
                                    (cons object1 objects))))
                   (or method
                       (error 'join "operation not available" objects op))))))

)
