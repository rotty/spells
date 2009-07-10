;;; tracing.sls --- Trace procedure invocations.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Trace procedures for debugging.
(library (spells tracing)
  (export trace-define trace-lambda trace-procedure)
  (import (rnrs base)
          (spells tracing compat))

  (define-syntax trace-procedure
    (syntax-rules ()
      ((trace-procedure <label> <proc-expr>)
       (let ((proc <proc-expr>))
         (trace-lambda <label> args
           (apply proc args))))))

  )
