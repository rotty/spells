;; sysutils.scm -*- scheme48-package: spells.sysutils -*-
;; Copyright (C) 2005 by Jose Antonio Ortega 

;; Author: Jose Antonio Ortega <jao@gnu.org>  
;; Start date: Wed Dec 28, 2005 01:34 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;; 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code: 

;;@ Returns the value of the environment variable @1 or #f if the
;; variable is not set.
(define (lookup-environment-variable str) (proc-to-be-defined))

;;@ Returns an alist containing the current process environment in the
;; form of @code{(VARIABLE-NAME . VARIABLE-VALUE)} string pairs.
(define (current-process-environment) (proc-to-be-defined))

;;@ Returns a new alist which the current process environment with the
;; alist @1, replacing any variable already present in the current
;; environment.
(define (extend-process-environment env) (proc-to-be-defined))

;;@ These procedures return strings that are supposed to identify the
;; current OS and machine. The POSIX standard does not indicate the
;; format of the strings.
(define (os-name) (proc-to-be-defined))
(define (os-node-name) (proc-to-be-defined))
(define (os-release-name) (proc-to-be-defined))
(define (os-version-name) (proc-to-be-defined))
(define (machine-name) (proc-to-be-defined))


;;; sysutils.scm ends here
