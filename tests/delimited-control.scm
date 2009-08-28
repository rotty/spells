;;; delimited-control.scm ---

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Code adapted from test section of
;; <http://okmij.org/ftp/Scheme/delim-control-n.scm>

;;; Code:


(define-test-suite control-tests
  "Delimited control")

(define-test-suite (control-tests.shift/reset control-tests)
  "shift/reset")

(define-test-case control-tests.shift/reset basics ()
  (test-eqv 117
    (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
  (test-eqv 60
    (* 10 (reset (* 2 (shift g (reset
                                (* 5 (shift f (+ (f 1) 1)))))))))
  (test-eqv 121
    (let ((f (lambda (x) (shift k (k (k x))))))
      (+ 1 (reset (+ 10 (f 100))))))
  (test-equal '(a)
    (reset
     (let ((x (shift f (cons 'a (f '())))))
       (shift g x)))))

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(define (shift* p) (shift f (p f)))

(define-test-case control-tests.shift/reset ss ()
  (test-eqv #t
    (reset (let ((x 'abcde))
             (eq? x ((shift* shift*) x))))))

(define (traverse/shift xs)
  (define (visit xs)
    (if (null? xs)
        '()
        (visit (shift k (cons (car xs) (k (cdr xs)))))))
  (reset (visit xs)))

(define-test-case control-tests.shift/reset traverse ()
  (test-equal '(1 2 3 4 5)
    (traverse/shift '(1 2 3 4 5))))


(define-test-suite (control-tests.prompt/control control-tests)
  "prompt/control")

(define-test-case control-tests.prompt/control basics ()
  (test-eqv 117
    (+ 10 (prompt (+ 2 (control k (+ 100 (k (k 3))))))))
  (test-eq '()
    (prompt (let ((x (control f (cons 'a (f '()))))) (control g x))))
  (test-eqv 2
    (prompt ((lambda (x) (control l 2)) (control l (+ 1 (l 0))))))
  (test-equal '(a)
    (prompt (control f (cons 'a (f '())))))
  (test-equal '(a)
    (prompt (let ((x (control f (cons 'a (f '()))))) (control g (g x))))))

(define-test-case control-tests.prompt/control cc ()
  (test-eqv #t
    (prompt
     (let ((x 'abcde))
       (eq? x ((control k (control k2 (k k2))) x))))))

;; Example from Sitaram, Felleisen
(define-test-case control-tests.prompt/control ex1 ()
  (test-eqv 42
    (let ((g (prompt (* 2 (control k k)))))
    (* 3 (prompt (* 5 (abort (g 7))))))))


;; Olivier Danvy's puzzle

(define (traverse/control xs)
  (define (visit xs)
    (if (null? xs)
        '()
        (visit (control k (cons (car xs) (k (cdr xs)))))))
  (prompt (visit xs)))

(define-test-case control-tests.shift/reset traverse ()
  (test-equal '(5 4 3 2 1)
    (traverse/control '(1 2 3 4 5))))



(define-test-suite (control-tests.prompt0/control0 control-tests)
  "prompt0/control0")

(define-test-case control-tests.prompt0/control0 basics ()
  (test-eqv 117
    (+ 10 (prompt0 (+ 2 (control0 k (+ 100 (k (k 3))))))))
  (test-eqv '()
    (prompt0 (prompt0
              (let ((x (control0 f (cons 'a (f '()))))) (control0 g x))))))


(define-test-suite (control-tests.shift0/reset0 control-tests)
  "shift0/reset0")

(define-test-case control-tests.shift0/reset0 basics ()
  (test-eqv 117
    (+ 10 (reset0 (+ 2 (shift0 k (+ 100 (k (k 3))))))))
  (test-eq '()
    (reset0 (cons 'a (prompt0 (shift0 f (shift0 g '())))))))



(run-test-suite control-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
