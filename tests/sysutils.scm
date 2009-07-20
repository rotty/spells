;; sysutils.scm -*- scheme48-package: nil -*-
;; Copyright (C) 2005, 2006-2007, 2009 by Jose Antonio Ortega 

;; Author: Jose Antonio Ortega <jao@gnu.org>  
;; Start date: Wed Dec 28, 2005 02:08 

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite sysutils-tests
  "System utilities")

(define-test-suite (sysutils-tests.environment sysutils-tests)
  "Environment variables")

(define-test-case sysutils-tests.environment lookup ()
  (test-equal #f
    (lookup-environment-variable "_AN_IMPR0B4BL3__NMAE_F0R_1__VAR__"))
  (test-equal #t (string? (lookup-environment-variable "PATH"))))

(define-test-case sysutils-tests.environment extending ()
  (test-equal #t
    (let* ((path (lookup-environment-variable "PATH"))
           (home (string-append (lookup-environment-variable "HOME") "_"))
           (new-env (extend-process-environment
                     `(("FOO" . "BAR") ("HOME" . ,home)))))
      (and (equal? (assoc "FOO" new-env) '("FOO" . "BAR"))
           (= (count (lambda (pr) (string=? (car pr) "HOME")) new-env) 1)
           (equal? (assoc "HOME" new-env) `("HOME" . ,home))
           (equal? (assoc "PATH" new-env) `("PATH" . ,path))))))

(run-test-suite sysutils-tests)
