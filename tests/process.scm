;; process.scm -- unit tests for process.scm

;; Copyright (C) 2005-2010, 2012 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann
;; Start date: Fri May 20, 2005 23:22

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (rnrs sorting)
        (srfi :8 receive)
        (only (srfi :13 strings) string-join)
        (spells string-utils)
        (spells process)
        (spells sysutils)
        (wak trc-testing))

(define (lines . strings)
  (list 0 #f (string-join strings (string #\newline) 'suffix)))

(define (sort-environment env)
  (list-sort (lambda (entry1 entry2)
               (string<? (car entry1) (car entry2)))
             env))

(define-test-suite process-tests
  "Process interface")

(define-test-suite (process-tests.run-process process-tests)
  "run-process")

(define-test-case process-tests.run-process echo ()
  (test-equal (lines "foo")
    (receive results (run-process/string #f "/bin/echo" "foo")
      results)))

(define-test-case process-tests.run-process echo/-e ()
  (test-equal (lines "foo" "bar")
    (receive results (run-process/string #f "/bin/echo" "-e" "foo\\nbar")
      results)))

(define-test-case process-tests.run-process echo/-e/lines ()
  (test-equal (list 0 #f '("foo" "bar"))
    (receive results (run-process/lines #f "/bin/echo" "-e" "foo\\nbar")
      results)))

(define-test-case process-tests.run-process echo/sexps ()
  (test-equal (list 0 #f '((hello world)))
    (receive results (run-process/sexps #f "/bin/echo" "(hello world)")
      results)))

(define-test-case process-tests.run-process env ()
  (test-equal (list 0 #f '("HELLO=WORLD"))
    (receive results (run-process/lines '(("HELLO" . "WORLD")) "/usr/bin/env")
      results)))

(define-test-case process-tests.run-process env/extend ()
  (let ((env (extend-process-environment '(("HELLO" . "WORLD")))))
    (test-equal (list 0 #f (sort-environment env))
      (receive (status signal lines)
               (run-process/lines env "/usr/bin/env")
        (list status
              signal
              (sort-environment
               (map (lambda (line)
                      (let ((parts (string-split line #\= 2)))
                        (cons (car parts) (cadr parts))))
                    lines)))))))

(define-test-suite (process-tests.call-with-process-output process-tests)
  "call-with-process-output")

(define-test-case process-tests.call-with-process-output echo/sexp ()
  (test-equal (list 0 #f '(hello world))
    (receive results (call-with-process-output #f '("/bin/echo" "(hello world)") read)
      results)))

(define-test-case process-tests.call-with-process-output sleep/echo ()
  (test-equal (list 0 #f "foo")
    (receive results (call-with-process-output
                         #f '("/bin/sh" "-c" "sleep 1 && echo foo")
                       get-line)
      results)))

(define-test-case process-tests run-shell-command ()
  (test-equal '(42 #f)
    (receive results (run-shell-command "exit 42")
      results)))

(run-test-suite process-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
