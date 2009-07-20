;;; cut.scm --- Confidence tests for SRFI-26

;; Copyright (C) Sebastian Egner (2002). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE

;;; Commentary:

;; Adapted by Andreas Rottmann for inclusion in spells.

;;; Code:

(define-test-suite srfi-26-tests
  "SRFI 26 confidence tests")

(define-test-case srfi-26-tests cut ()
  (test-equal '() ((cut list)))
  (test-equal '() ((cut list <...>)))
  (test-equal '(1) ((cut list 1)))
  (test-equal '(1) ((cut list <>) 1))
  (test-equal '(1) ((cut list <...>) 1))
  (test-equal '(1 2) ((cut list 1 2)))
  (test-equal '(1 2) ((cut list 1 <>) 2))
  (test-equal '(1 2) ((cut list 1 <...>) 2))
  (test-equal '(1 2 3 4) ((cut list 1 <...>) 2 3 4))
  (test-equal '(1 2 3 4) ((cut list 1 <> 3 <>) 2 4))
  (test-equal '(1 2 3 4 5 6) ((cut list 1 <> 3 <...>) 2 4 5 6))
  (test-equal '(ok) (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)))
  (test-equal 2
    (let ((a 0))
      (map (cut + (begin (set! a (+ a 1)) a) <>)
           '(1 2))
      a)))

(define-test-case srfi-26-tests cute ()
  (test-equal '() ((cute list)))
  (test-equal '() ((cute list <...>)))
  (test-equal '(1) ((cute list 1)))
  (test-equal '(1) ((cute list <>) 1))
  (test-equal '(1) ((cute list <...>) 1))
  (test-equal '(1 2) ((cute list 1 2)))
  (test-equal '(1 2) ((cute list 1 <>) 2))
  (test-equal '(1 2) ((cute list 1 <...>) 2))
  (test-equal '(1 2 3 4) ((cute list 1 <...>) 2 3 4))
  (test-equal '(1 2 3 4) ((cute list 1 <> 3 <>) 2 4))
  (test-equal '(1 2 3 4 5 6) ((cute list 1 <> 3 <...>) 2 4 5 6))
  (test-equal 1
    (let ((a 0))
      (map (cute + (begin (set! a (+ a 1)) a) <>)
           '(1 2))
      a)))

(run-test-suite srfi-26-tests)
