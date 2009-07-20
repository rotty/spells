;;; algebraic-types.sls --- EOPL-style algebraic datatypes

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells algebraic-types)
  (export define-datatype cases)
  (import (rnrs)
          (for (spells algebraic-types helpers) expand))

(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      ((k name (variant (field ...)) ...)
       #'(begin
           (define-record-type variant
             (fields field ...))
           ...
           (define-syntax name
             (expand-datatype-dispatcher
              (syntax->datum #'name)
              (syntax->datum #'((variant field ...) ...)))))))))

(define-syntax cases
  (syntax-rules ()
    ((_ datatype clause ...)
     (datatype "cases" clause ...))))

)

