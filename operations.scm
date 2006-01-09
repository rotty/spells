;; -*- Mode: Scheme; scheme48-package: spells.operations; -*-
;;
;; Copyright (C) 2006 by Free Software Foundation, Inc.

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
(define-syntax method-clauses->alist
  (syntax-rules ()
    ((method-clauses->alist ((?op ?param ...) ?body ...) ...)
     (list (cons ?op (lambda (?param ...) ?body ...)) ...))))

(define-syntax object
  (syntax-rules ()
    ((object ?proc ?method-clause ...)
     (make-object ?proc (method-clauses->alist ?method-clause ...)))))

(define %get-handler (list '%get-method))

(define (make-object proc handler)
  (lambda args
    (cond ((null? args)
           (proc))
          ((and (null? (cdr args))
                (eq? (car args) %get-handler))
           handler)
          (else
           (apply proc args)))))

(define-syntax operation
  (syntax-rules ()
    ((operation ?default ?method-clause ...)
     (make-operation ?default (method-clauses->alist ?method-clause ...)))))

(define (make-operation default handler)
  (letrec ((op (make-object
                (lambda (obj . args)
                  (cond ((and (procedure? obj) ((obj %get-handler) op))
                         => (lambda (method)
                              (apply method obj args)))
                        (default
                          (apply default obj args))
                        (else
                         (error "operation has not available" obj op))))
                handler)))
    op))

(define-syntax define-operation
  (syntax-rules ()
    ((define-operation (?name ?arg ...))
     (define ?name (operation #f)))
    ((define-operation (?name ?arg ...) ?body1 ?body ...)
     (define ?name (operation (lambda (?arg ...) ?body ...))))))
