;; parameter.scm -- Unit tests for spells.parameter (SRFI 39)

;; Copyright (C) 2005, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 30, 2005 22:43

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite parameter-tests
  "SRFI-39 parameters")

(define-test-case parameter-tests mutation ()
  (test-equal 10
    (let ((p (make-parameter 5))) (p 10) (p))))

(define-test-case parameter-tests parameterize ()
  (test-equal '(10 5)
    (let ((p (make-parameter 5)))
      (list
       (parameterize ((p 10))
         (p))
       (p)))))

(run-test-suite parameter-tests)

;;; parameter.scm ends here
