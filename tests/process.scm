;; process.scm -- unit tests for process.scm
;; arch-tag: 1a073af6-85ae-4ca1-86f1-03a7bbaae8f4

;; Copyright (C) 2005-2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann
;; Start date: Fri May 20, 2005 23:22

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define (lines . strings)
  (values 0 #f (string-join strings (string #\newline) 'suffix)))

(define-test-suite process-tests
  "Process interface")

(define-test-suite (process-tests.run-process process-tests)
  "run-process")


(define-test-case process-tests.run-process echo ()
  (test-equal (lines "foo")
    (run-process/string #f "/bin/echo" "foo")))

(define-test-case process-tests.run-process echo/-e ()
  (test-equal (lines "foo" "bar")
    (run-process/string #f "/bin/echo" "-e" "foo\\nbar")))

(define-test-case process-tests.run-process echo/-e/lines ()
  (test-equal (list 0 #f '("foo" "bar"))
    (receive results (run-process/lines #f "/bin/echo" "-e" "foo\\nbar")
      results)))

(define-test-case process-tests.run-process echo/sexps ()
  (test-equal (list 0 #f '((hello world)))
    (receive results (run-process/sexps #f "/bin/echo" "(hello world)")
      results)))

(define-test-suite (process-tests.call-with-process-output process-tests)
  "call-with-process-output")

(define-test-case process-tests.call-with-process-output echo/sexp ()
  (test-equal (list 0 #f '(hello world))
    (receive results (call-with-process-output #f '("/bin/echo" "(hello world)") read)
      results)))

(define-test-case process-tests.call-with-process-output sleep/echo ()
  (test-equal (list 0 #f "foo")
    (receive results (call-with-process-output #f '("/bin/sh" "-c" "sleep 1 && echo foo")
                       get-line)
      results)))

(run-test-suite process-tests)

;;; process.scm ends here
