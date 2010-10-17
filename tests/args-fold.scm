;;; args-fold.scm --- Tests for the command line argument processor

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
(import (rnrs)
        (spells alist)
        (wak trc-testing)
        (spells args-fold))

(define-test-suite args-tests
  "command argument parsing")

(define (process-option option name arg seed)
  (cons (cons name arg) seed))

(define (unrecognized-option option name arg seed)
  (cons (list 'unrecognized name arg) seed))

(define (process-operand operand seed)
  (cons (cons 'operands (cons operand (assq-ref seed 'operands)))
        (remp (lambda (entry)
                (eq? 'operands (car entry)))
              seed)))

(define-test-suite (args-tests.fold args-tests)
  "args-fold")

(define-test-case args-tests.fold without-options ()
  (test-equal '((operands))
    (args-fold '() '() unrecognized-option process-operand '((operands))))
  (test-equal '((operands "baz" "bar" "foo"))
    (args-fold '("foo" "bar" "baz")
               '()
               unrecognized-option
               process-operand
               '((operands))))
  (test-equal '((unrecognized "baz" #f)
                (operands "bar" "foo"))
    (args-fold '("foo" "bar" "--baz")
               '()
               unrecognized-option
               process-operand
               '((operands)))))

(define-test-case args-tests.fold long-equal-sign ()
  (test-equal '((operands "frotz" "baz")
                ("foo" . "bar"))
    (args-fold '("--foo=bar" "baz" "frotz")
               (list (option '("foo") 'file process-option))
               unrecognized-option
               process-operand
               '((operands)))))

(define-test-case args-tests.fold non-strings ()
  (test-equal '((operands "frotz" #("baz"))
                ("foo" . "bar"))
    (args-fold '("--foo=bar" #("baz") "frotz")
               (list (option '("foo") 'file process-option))
               unrecognized-option
               process-operand
               '((operands)))))

(define-test-suite (args-tests.fold* args-tests)
  "args-fold*")

(define-test-case args-tests.fold* terminator ()
  (test-equal '((operands "bar" "foo")
                ("args" . #f))
    (args-fold* '("--args" "foo" "bar")
                (list (option '("args") #f #f #t process-option))
                #f
                unrecognized-option
                process-operand
                '((operands)))))

(define-test-case args-tests.fold* stop-early ()
  (test-equal '((operands "bar" "--foo" "test")
                ("frob" . "5"))
    (args-fold* '("--frob" "5" "test" "--foo" "bar")
                (list (option '("frob") #t #f process-option)
                      (option '("foo") #f #f process-option))
                #t
                unrecognized-option
                process-operand
                '((operands)))))

(run-test-suite args-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
