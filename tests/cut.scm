; CONFIDENCE TEST FOR IMPLEMENTATION OF SRFI-26
; =============================================
;
; Sebastian.Egner@philips.com, 3-Jun-2002.
;
; This file checks a few assertions about the implementation.
; If you run it and no error message is issued, the implementation
; is correct on the cases that have been tested.
;
; compliance:
;   Scheme R5RS with
;     SRFI-23: error
;
; loading this file into Scheme 48 0.57 after 'cut.scm' has been loaded:
;   ,open srfi-23
;   ,load check.scm

; (check expr)
;    evals expr and issues an error if it is not #t.
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
