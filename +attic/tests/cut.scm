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
(testeez
 "confidence tests"
 ;; cuts
 (test/equal "test" ((cut list)) '())
 (test/equal "test" ((cut list <...>)) '())
 (test/equal "test" ((cut list 1)) '(1))
 (test/equal "test" ((cut list <>) 1) '(1))
 (test/equal "test" ((cut list <...>) 1) '(1))
 (test/equal "test" ((cut list 1 2)) '(1 2))
 (test/equal "test" ((cut list 1 <>) 2) '(1 2))
 (test/equal "test" ((cut list 1 <...>) 2) '(1 2))
 (test/equal "test" ((cut list 1 <...>) 2 3 4) '(1 2 3 4))
 (test/equal "test" ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4))
 (test/equal "test" ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
 (test/equal "test" (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)) '(ok))
 (test/equal "test" 
  (let ((a 0))
    (map (cut + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  2)
                                        ; cutes
 (test/equal "test" ((cute list)) '())
 (test/equal "test" ((cute list <...>)) '())
 (test/equal "test" ((cute list 1)) '(1))
 (test/equal "test" ((cute list <>) 1) '(1))
 (test/equal "test" ((cute list <...>) 1) '(1))
 (test/equal "test" ((cute list 1 2)) '(1 2))
 (test/equal "test" ((cute list 1 <>) 2) '(1 2))
 (test/equal "test" ((cute list 1 <...>) 2) '(1 2))
 (test/equal "test" ((cute list 1 <...>) 2 3 4) '(1 2 3 4))
 (test/equal "test" ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4))
 (test/equal "test" ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
 (test/equal "test" 
  (let ((a 0))
    (map (cute + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  1))

;; arch-tag: 4e8fa92a-b805-4784-912b-b1c00b0a54b2
