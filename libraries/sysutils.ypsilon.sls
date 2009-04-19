;;; sysutils.ypsilon.sls --- Ypsilon sysutils

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells sysutils)
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          find-exec-path
          host-info)
  (import (rnrs base)
          (rnrs records syntactic)
          (srfi :8 receive)
          (spells string-utils)
          (spells filesys)
          (spells sysutils run-uname)
          (only (core)
                architecture-feature
                getenv
                process-environment->alist)
          (only (ypsilon ffi) on-posix))

  (define (todo-proc who)
    (lambda args
      (error who "please implement me!")))

  (define (find-exec-path prog)
    (let ((paths (string-split (getenv "PATH") #\:)))
      (find-file prog paths file-executable?)))

  (define lookup-environment-variable getenv)

  (define current-process-environment process-environment->alist)

  (define extend-process-environment (todo-proc 'extend-process-environment))

  (define (host-info)
    (let ((os (architecture-feature 'operating-system)))
      (values
       (architecture-feature 'machine-hardware)
       "unknown"
       (cond ((string=? os "linux") "linux-gnu")
             (else                  os)))))

  )
