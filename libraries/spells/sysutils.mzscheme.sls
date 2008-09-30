;; Copyright (C) 2005, 2007, 2008 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Jun 12, 2005 22:09

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
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

;;; sysutils.scm ends here
