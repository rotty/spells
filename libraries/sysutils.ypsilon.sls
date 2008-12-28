;;; sysutils.ypsilon.sls --- Ypsilon sysutils

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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
          os-name
          os-node-name
          os-release-name
          os-version-name
          machine-name)
  (import (rnrs base)
          (only (core) getenv))

  (define (todo-proc who)
    (lambda args
      (error who "please implement me!")))
  
  (define lookup-environment-variable getenv)
  
  (define current-process-environment (todo-proc 'current-process-environment))
  
  (define extend-process-environment (todo-proc 'extend-process-environment))
  
  (define find-exec-path (todo-proc 'find-exec-path))
  
  (define os-name (todo-proc 'os-name))
  
  (define os-node-name (todo-proc 'os-node-name))
  
  (define os-release-name (todo-proc 'os-release-name))
  
  (define os-version-name (todo-proc 'os-version-name))
  
  (define machine-name (todo-proc 'machine-name))
  
  )
