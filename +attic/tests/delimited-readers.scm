;; delimited-readers.scm -- unit tests for delimited-readers.scm
;; arch-tag: 9eded783-24d0-4fac-8044-b1bd464815de

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Fri Nov 06, 2005 14:32

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(define (make-lines-port)
  (open-input-string (string-join '("A line"
                                    "Another line")
                                  (string #\newline))))

(define (make-paragraph-port)
  (open-input-string (string-join '("A line"
                                    "on the first paragraph"
                                    "  "
                                    "Another paragraph")
                                  (string #\newline))))

(define (s . args)
  (apply string-append
         (map (lambda (s) (cond ((char? s) (string s))
                                (else s)))
              args)))

(let ((port (make-lines-port)))
  (testeez
   "read-line/simple"
   (test/equal "simple-1" (read-line port) "A line")
   (test/equal "simple-2" (read-line port) "Another line")
   (test-true "eof" (eof-object? (read-line port)))))

(let ((port (make-lines-port)))
  (testeez
   "read-line/concat"
   (test/equal "concat-1" (read-line port 'concat) (s "A line" #\newline))
   (test/equal "concat-2" (read-line port 'concat) (s "Another line"))
   (test-true "eof" (eof-object? (read-line port 'concat)))))

(let ((port (make-lines-port)))
  (testeez
   "read-line/split"
   (test/equal "split-1" (read-line port 'split) (values "A line" #\newline))
   (test/equal "split-2" (read-line port 'split) (values "Another line" (eof-object)))
   (test/equal "eof" (read-line port 'split) (values (eof-object) (eof-object)))))

(let ((port (make-lines-port)))
  (testeez
   "read-line/peek"
   (test/equal "peek-1-line" (read-line port 'peek) "A line")
   (test/equal "peek-1-char" (read-char port) #\newline)
   (test/equal "peek-2-line" (read-line port 'peek) "Another line")
   (test-true "peek-2-char" (eof-object? (read-char port)))
   (test-true "eof" (eof-object? (read-line port 'peek)))))

(let ((port (make-paragraph-port)))
  (testeez
   "read-paragraph/simple"
   (test/equal "simple-1" (read-paragraph port) (s "A line" #\newline
                                                   "on the first paragraph" #\newline))
   (test/equal "simple-2" (read-paragraph port) "Another paragraph")
   (test-true "eof" (eof-object? (read-paragraph port)))))