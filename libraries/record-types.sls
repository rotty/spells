;;; record-types.sls --- Record types.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This file is a kludge, as it uses define-macro, but that's way
;; easier than rewriting Riastradh's define-record-type* expander
;; (which is implemented as an explicit-renaming macro) to use
;; syntax-case. In practice, the unhygiene should not be much of
;; problem as define-record-type* will probably only be used at the
;; top-level.

;;; Code:
#!r6rs

(library (spells record-types)
  (export define-record-type
          define-record-type*
          define-functional-fields
          define-record-discloser)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) run expand)
          (for (rnrs lists) expand)
          (rnrs records procedural)
          (rnrs io simple)
          (srfi :8 receive)
          (spells define-values)
          (for (spells record-types expand-drt) expand))

  (define-syntax define-macro
    (lambda (stx)
      (syntax-case stx ()
        ((_ (macro . args) . body)
         (syntax (define-macro macro (lambda args . body))))
        ((_ macro transformer)
         (syntax
          (define-syntax macro
            (lambda (stx2)
              (let ((v (syntax->datum stx2)))
                (datum->syntax
                 ;; we need the *identifier* of the macro call
                 ;; (there is probably a smarter way of extracting that ...)
                 (syntax-case stx2 () ((name . more) (syntax name)))
                 (apply transformer (cdr v)))))))))))

  (define-macro (define-record-type* . forms)
    (expand-define-record-type* (cons 'define-record-type* forms)
                                (lambda (x) x)
                                eq?))

  (define-macro (define-functional-fields . forms)
    (expand-define-functional-fields (cons 'define-record-type* forms)
                                     (lambda (x) x)
                                     eq?))

  ;; How to implement this?
  (define-syntax define-record-discloser
    (syntax-rules ()
      ((define-record-discloser type proc)
       #t)))

  (define-syntax define-record-type
    (lambda (form)
      (define (iota n)
        (let loop ((result '()) (n (- n 1)))
          (if (< n 0)
              result
              (loop (cons n result) (- n 1)))))
      (define (field-spec-has-mutator? spec)
        (= (length spec) 3))
      (define (extract-field-accessor-names field-specs k)
        (datum->syntax k (apply append (map cdr field-specs))))
      (define (create-field-accessors field-specs k)
        (apply append (map (lambda (spec i)
                             (cons #`(record-accessor rtd #,i)
                                   (if (field-spec-has-mutator? spec)
                                       (list #`(record-mutator rtd #,i))
                                       '())))
                           field-specs
                           (iota (length field-specs)))))
      (define (field-vector field-specs)
        (list->vector (map (lambda (spec)
                             (if (field-spec-has-mutator? spec)
                                 `(mutable ,(car spec))
                                 `(immutable ,(car spec))))
                           field-specs)))
      (define (make-protocol field-tags field-specs k)
        (let ((field-tag-names (syntax->datum field-tags)))
          #`(lambda (p) (lambda #,field-tags
                          (p #,@(map (lambda (spec)
                                       (let ((tag-pair (memq (car spec) field-tag-names)))
                                         (if tag-pair
                                             (datum->syntax k (car tag-pair))
                                             #f)))
                                     field-specs))))))
      (syntax-case form ()
        ((k <type-name>
            (<constructor-name> <field-tag> ...)
            <predicate-name>
            <field-spec> ...)
         (let ((field-specs (syntax->datum #'(<field-spec> ...))))
           (let ((field-accessor-names (extract-field-accessor-names field-specs #'k))
                 (field-accessors (create-field-accessors field-specs #'k))
                 (protocol (make-protocol #'(<field-tag> ...) field-specs #'k)))
             (with-syntax (((accessor-name ...) field-accessor-names)
                           ((accessor ...) field-accessors)
                           (protocol protocol))
               #`(define-values (<constructor-name> <predicate-name> accessor-name ...)
                   (let* ((rtd (make-record-type-descriptor '<type-name> #f #f #f #f
                                                            '#,(datum->syntax #'k (field-vector field-specs))))
                          (cd (make-record-constructor-descriptor
                               rtd #f protocol)))
                     (values (record-constructor cd) (record-predicate rtd) accessor ...)))))))))))
