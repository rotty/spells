;;; include.sls --- Include scheme source code.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells include)
  (export include-file
          include-file/downcase)
  (import (rnrs)
          (for (spells tracing) expand)
          (for (only (spells include compat)
                     annotation?
                     annotation-expression) expand)
          (for (spells include helpers) expand))

  (define-syntax include-file
    (lambda (stx)
      (syntax-case stx ()
        ((k <path>)
         (include-file/aux 'include-file #'k (syntax->datum #'<path>) values)))))

  (define-syntax include-file/downcase
    (lambda (stx)
      ;; This loses all the annotations, but Ikarus provides no way to
      ;; (re-)construct annotation objects ATM.
      (define (downcase thing)
        (let ((form (if (annotation? thing)
                        (annotation-expression thing)
                        thing)))
          (cond ((symbol? form)
                 (string->symbol (string-downcase (symbol->string form))))
                ((pair? form)
                 (cons (downcase (car form))
                       (downcase (cdr form))))
                (else
                 thing))))
      (syntax-case stx ()
        ((k <path>)
         (include-file/aux 'include-file #'k (syntax->datum #'<path>) downcase)))))
  
)
