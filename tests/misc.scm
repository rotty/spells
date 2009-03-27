;; misc.scm -- Unit tests for (spells misc)
;; arch-tag: 4822bd68-4e16-4bbd-bca4-42277a933717

;; Copyright (C) 2005, 2008, 2009 by Jose Antonio Ortega Ruiz <jao@gnu.org>

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Jun 13, 2005 07:13

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite misc-tests
  "Misc functions")

(define-test-case misc-tests sort-list ()
  (test-equal '(1 3 4 4)
    (sort-list '(3 1 4 4) <))
  (test-equal '("z" "bc" "b" "a")
    (sort-list '("a" "bc" "b" "z") string>?)))

(define-test-case misc-tests and-map ()
  (test-equal #t (and-map (lambda (x) (not x)) '()))
  (test-equal 3 (and-map (lambda (x) x) '(2 #t 3)))
  (test-equal 4 (and-map (lambda (x) x) '(#t #t 3 4)))
  (test-equal #f (and-map (lambda (x) (< x 10)) '(3 4 5 13 1))))

(define-test-case misc-tests or-map ()
  (test-equal #f (or-map (lambda (x) (not x)) '()))
  (test-equal #t (or-map (lambda (x) (not x)) '(#t #t #t #f #t)))
  (test-equal #t (or-map (lambda (x) (> x 5)) '(1 3 2 3 34 12))))

(define-test-case misc-tests and=> ()
  (let ((alist '((foo 42)
                 (cthulu 666))))
    (test-equal #f (and=> (assq 'qux alist) cadr))
    (test-equal 42 (and=> (assq 'foo alist) cadr))))

(define-test-case misc-tests topological-sort ()
  (test-equal '(foo bar baz)
    (topological-sort '((foo bar) (bar baz) (baz))))
  (test-one-of equal? '((foo bar baz)
                        (baz foo bar)
                        (foo baz bar))
    (topological-sort '((foo bar) (bar) (baz)))))

(run-test-suite misc-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:

