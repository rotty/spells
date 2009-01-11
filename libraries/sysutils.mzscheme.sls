;; Copyright (C) 2005, 2007 Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Jose Antonio Ortega Ruiz <jao@gnu.or>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells sysutils)
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          find-exec-path
          os-name
          os-node-name
          os-release-name
          os-version-name
          machine-name)

  (import (rnrs base)
          (only (mzscheme) getenv system-type find-executable-path))

  (define lookup-environment-variable getenv)

  (define (extend-process-environment alist)
    (error "EXTEND-PROCESS-ENVIRONMENT not implemented on mzscheme"))

  (define (current-process-environment)
    (error "CURRENT-PROCESS-ENVIRONMENT not implemented on mzscheme"))

  (define (find-exec-path prog)
    (find-executable-path prog #f))

  (define (os-name) (symbol->string (system-type 'os)))
  (define (os-node-name) "unknown")
  (define (os-release-name) "unknown")
  (define (os-version-name) "unknown")
  (define (machine-name) (system-type 'machine)))
