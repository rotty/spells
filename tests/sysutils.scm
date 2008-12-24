;; sysutils.scm -*- scheme48-package: nil -*-
;; Copyright (C) 2005, 2006-2007 by Jose Antonio Ortega 

;; Author: Jose Antonio Ortega <jao@gnu.org>  
;; sysutils.scm -- Unit tests for spells.sysutils

;; Copyright (C) 2005 by Free Software Foundation, Inc.
;; Start date: Wed Dec 28, 2005 02:08 

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
(all-dialects-except mzscheme ikarus)

(testeez "environment"
  (test-false "lookup"
    (lookup-environment-variable "_AN_IMPR0B4BL3__NMAE_F0R_1__VAR__"))
  (test-true "lookup" (string? (lookup-environment-variable "PATH")))
  (test-true "extending"
    (let* ((path (lookup-environment-variable "PATH"))
           (home (string-append (lookup-environment-variable "HOME") "_"))
           (new-env (extend-process-environment
                     `(("FOO" . "BAR") ("HOME" . ,home)))))
      (and (equal? (assoc "FOO" new-env) '("FOO" . "BAR"))
           (= (count (lambda (pr) (string=? (car pr) "HOME")) new-env) 1)
           (equal? (assoc "HOME" new-env) `("HOME" . ,home))
           (equal? (assoc "PATH" new-env) `("PATH" . ,path))))))

;;; sysutils.scm ends here
