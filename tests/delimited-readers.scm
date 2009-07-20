;; delimited-readers.scm -- unit tests for delimited-readers.scm

;; Copyright (C) 2005, 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Fri Nov 06, 2005 14:32

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define (make-lines-port)
  (open-string-input-port (string-join '("A line"
                                         "Another line")
                                       (string #\newline))))

(define (make-paragraph-port)
  (open-string-input-port (string-join '("A line"
                                         "on the first paragraph"
                                         "  "
                                         "Another paragraph")
                                       (string #\newline))))

(define (s . args)
  (apply string-append
         (map (lambda (s) (cond ((char? s) (string s))
                                (else s)))
              args)))

(define-test-suite rdelim-tests
  "Delimited readers")

(define-test-suite (rdelim-tests.read-line rdelim-tests)
  "read-line")

(define-test-case rdelim-tests.read-line simple ()
  (let ((port (make-lines-port)))
    (test-equal "A line" (read-line port))
    (test-equal "Another line" (read-line port))
    (test-equal #t (eof-object? (read-line port)))))

(define-test-case rdelim-tests.read-line concat ()
  (let ((port (make-lines-port)))
    (test-equal (s "A line" #\newline) (read-line port 'concat))
    (test-equal (s "Another line") (read-line port 'concat))
    (test-equal #t (eof-object? (read-line port 'concat)))))

(define-test-case rdelim-tests.read-line split ()
  (let ((port (make-lines-port)))
    (test-equal (list "A line" #\newline)
      (receive results (read-line port 'split)
        results))
    (test-equal (list "Another line" (eof-object))
      (receive results (read-line port 'split)
        results))
    (test-equal (list (eof-object) (eof-object))
      (receive results (read-line port 'split)
        results))))

(define-test-case rdelim-tests.read-line peek ()
  (let ((port (make-lines-port)))
    (test-equal "A line" (read-line port 'peek))
    (test-equal #\newline (read-char port))
    (test-equal "Another line" (read-line port 'peek))
    (test-equal #t (eof-object? (read-char port)))
    (test-equal #t (eof-object? (read-line port 'peek)))))

(define-test-suite (rdelim-tests.read-paragraph rdelim-tests)
  "read-paragraph")

(define-test-case rdelim-tests.read-paragraph simple ()
  (let ((port (make-paragraph-port)))
    (test-equal (s "A line" #\newline
                   "on the first paragraph" #\newline)
      (read-paragraph port))
    (test-equal "Another paragraph" (read-paragraph port))
    (test-equal #t (eof-object? (read-paragraph port)))))

(run-test-suite rdelim-tests)
