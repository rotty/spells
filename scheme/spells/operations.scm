;; -*- Mode: Scheme; scheme48-package: spells.operations; -*-
;;
;; Copyright (C) 2006-2007 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Sat Jan  7 16:17:26 CET 2006

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; A generic dispatch system similiar to operations in T

;;; Code:

;; Auxiliary syntax
(define-syntax %method-clauses->handler
  (syntax-rules ()
    ((%method-clauses->handler ((?op ?param ...) ?body ...) ...)
     (let ((methods (list (cons ?op (lambda (?param ...) ?body ...)) ...)))
       (lambda (op)
         (cond ((assq op methods) => cdr)
               (else #f)))))))

(define-syntax object
  (syntax-rules ()
    ((object ?proc ?method-clause ...)
     (make-object ?proc (%method-clauses->handler ?method-clause ...)))))

(define (make-object proc handler)
  (annotate-procedure (or proc (lambda args (error "object is not applicable"))) handler))

(define-syntax operation
  (syntax-rules ()
    ((operation ?default ?method-clause ...)
     (make-operation ?default (%method-clauses->handler ?method-clause ...)))))

(define (make-operation default handler)
  (letrec ((op (make-object
                (lambda (obj . args)
                  (cond ((and (procedure? obj) ((procedure-annotation obj) op))
                         => (lambda (method)
                              (apply method obj args)))
                        (default
                          (apply default obj args))
                        (else
                         (error "operation is not available" obj op))))
                handler)))
    op))

(define-syntax define-operation
  (syntax-rules ()
    ((define-operation (?name ?arg ...))
     (define ?name (operation #f)))
    ((define-operation (?name ?arg ...) ?body1 ?body ...)
     (define ?name (operation (lambda (?arg ...) ?body1 ?body ...))))))

(define (join object1 . objects)
  (make-object object1
               (lambda (op)
                 (let ((method (any (lambda (o) ((procedure-annotation o) op))
                                    (cons object1 objects))))
                   (or method
                       (error "operation not available" objects op))))))
