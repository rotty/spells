;;; ports.scm --- Unit tests for (spells ports)

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-test-suite ports-tests
  "Port-related utilities")

(define-syntax test-row/col
  (syntax-rules ()
    ((_ row-expected col-expected tracker)
     (begin
       (test-equal row-expected (port-tracker-row tracker))
       (test-equal col-expected (port-tracker-column tracker))))))

(define-test-case ports-tests tracking ()
  (test-equal "hello foo\nbarish baz\nqux\nSome text"
    (call-with-string-output-port
      (lambda (port)
        (let* ((tracker (make-port-tracker port))
               (tport (port-tracker-port tracker)))
          (define (out s)
            (put-string tport s))
          (test-row/col 0 0 tracker)
          (out "hello")
          (test-row/col 0 5 tracker)
          (out " foo\n")
          (test-row/col 1 0 tracker)
          (out "bar")
          (test-row/col 1 3 tracker)
          (out "ish")
          (test-row/col 1 6 tracker)
          (out " baz\nqux\nSome text")
          (test-row/col 3 9 tracker))))))

(run-test-suite ports-tests)
