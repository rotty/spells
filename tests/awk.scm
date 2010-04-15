;;; awk.scm --- Tests for the awk loop macro

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2004 Christoph Hetz

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Originally written by Christoph Hetz, modified and extended by
;; Andreas Rottmann for inclusion in spells.

;;; Code:

(import (rnrs)
        (only (srfi :1) make-list iota)
        (only (srfi :13) string-join)
        (wak irregex)
        (spells ascii)
        (spells delimited-readers)
        (spells awk)
        (wak trc-testing))

;;; basic helper functions

(define (call-with-string-input-port s proc)
  (call-with-port (open-string-input-port s) proc))

(define (some-test-lines test)
  (string-append "ein paar testzeilen, um\n" 
                 test " zu prüfen:\n"
                 "EINE ZEILE GRO/3...\n" 
                 "eine zeile klein...\n"
                 "eine zeile mit zeichen...\n"
                 "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n"))

;;; tests

(define-test-suite awk-tests
  "AWK macro")

(define-test-case awk-tests counter-inc ()
  (let ((read '()))
    (call-with-string-input-port (string-join (make-list 9 "test-zeile") "\n")
                                 (lambda (in-port)
                                   (awk (read-line in-port) (line) counter ()
                                        (#t (set! read (cons counter read))))))
    (test-equal '(9 8 7 6 5 4 3 2 1) read)))

(define-test-case awk-tests int-test ()
  (let ((read '()))
    (call-with-string-input-port (string-join (make-list 9 "test-zeile") "\n")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (1 (set! read (cons 1 read)))
             (2 (set! read (cons 2 read)))
             (3 (set! read (cons 3 read)))
             (4 (set! read (cons 4 read)))
             (5 (set! read (cons 5 read)))
             (6 (set! read (cons 6 read)))
             (7 (set! read (cons 7 read)))
             (8 (set! read (cons 8 read)))
             (9 (set! read (cons 9 read)))
             (0 (set! read (cons 0 read))))))
    (test-equal '(9 8 7 6 5 4 3 2 1) read)))

(define-test-case awk-tests huge-line ()
  (let ((one-mb-line (make-string (* 1024 1024) #\a))
        (read '()))
    (call-with-string-input-port one-mb-line
      (lambda (in-port)
        (awk (read-line in-port) (line) c ()
             (#t (set! read line)))))
    (test-equal one-mb-line read)))

(define-test-case awk-tests special-signs ()
  (let ((strange-sign-line (list->string
                            ;; works with everything but line-feed (ascii 10)
                            (map integer->char (remv 10 (iota 256)))))
        (read '()))
    (call-with-string-input-port strange-sign-line
      (lambda (in-port)
       (awk (read-line in-port) (line) ()
            (#t (set! read line)))))
    (test-equal strange-sign-line read)))

(define-test-case awk-tests sre-expr-test ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "sre-expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) ()
             (("sre" "zu") (set! read (cons 'sre-zu read)))
             ("eine" (set! read (cons 'eine read)))
             ("EINE" (set! read (cons 'EINE read)))
             ((* "3") (set! read (cons '*3 read)))
             ((? "s") (set! read (cons '?s read)))
             ((+ "+") (set! read (cons 'plusplus read))))))
    ;;                 |z6         |z5                   |z4
    (test-equal (list 'plusplus '?s '*3 '?s '*3 'eine 'sre-zu '?s '*3 'eine 'sre-zu
                      ;;                 |z3           |z2             |z1             |
                      '?s '*3 'EINE '?s '*3 'sre-zu '?s '*3 'sre-zu)
      read)))

(define-test-case awk-tests when-bool-exp-test ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "when-bool-expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             ((when (= counter 1)) 
              (set! read (cons 'first-clause read)))
             ((when (equal? line 
                            "when-bool-expr-test zu prüfen:"))
              (set! read (cons 'second-clause read)))
             ((when (> counter 2))
              (set! read (cons 'third-clause read))))))
    (test-equal (list 'third-clause 'third-clause 'third-clause 
                      'third-clause 'second-clause 'first-clause)
      read)))

(define-test-case awk-tests expr-test ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             ("paar" (set! read (cons 'first-clause read)))
             ((equal? line
                      "expr-test zu prüfen:")
              (set! read (cons 'second-clause read)))
             ((> counter 5)
              (set! read (cons 'third-clause read)))
             ((+ "3")                                  
              (set! read (cons 'fourth-clause read))))))
    (test-equal '(third-clause fourth-clause second-clause first-clause)
      read)))

;; TODO: only for <test>s that were ok till now (int, when)
(define-test-case awk-tests several-bodys-in-clause ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (1 (set! read (cons 'clause-one-body-one read))
                (set! read (cons 'clause-one-body-two read))
                (set! read (cons 'clause-one-body-three read)))
             ((when (equal? line
                            "eine zeile klein..."))
              (set! read (cons 'clause-two-body-one read))
              (set! read (cons 'clause-two-body-two read))
              (set! read (cons 'clause-two-body-three read))))))
    (test-equal (list 'clause-two-body-three 'clause-two-body-two 'clause-two-body-one 
                      'clause-one-body-three 'clause-one-body-two 'clause-one-body-one)
      read)))

;; TODO: only ok <test>s ... s.u.
(define-test-case awk-tests range-wo-begin-wo-end ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (range 1 3 (set! read (cons 'first-clause read)))
             (range (when (equal? line 
                                  "EINE ZEILE GRO/3..."))
                    (when (equal? line
                                  "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
                    (set! read (cons 'second-clause read)))
             (range (when (equal? line
                                  "expr-test zu prüfen:"))
                    4
                    (set! read (cons 'third-clause read))))))
    (test-equal (list 'second-clause 'second-clause 'third-clause 'first-clause)
      read)))

;; TODO: only ok <test>s ... s.u.
(define-test-case awk-tests range-w-begin-wo-end ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (:range 1 3 (set! read (cons 'first-clause read)))
             (:range (when (equal? line 
                                   "EINE ZEILE GRO/3..."))
                     (when (equal? line
                                   "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
                     (set! read (cons 'second-clause read)))
             (:range (when (equal? line
                                   "expr-test zu prüfen:"))
                     4
                     (set! read (cons 'third-clause read))))))
    (test-equal (list 'second-clause 'second-clause 'third-clause 'second-clause 
                      'third-clause 'first-clause 'first-clause)
      read)))

;; TODO: only ok <test>s ... s.u.
(define-test-case awk-tests range-wo-begin-w-end ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (range: 1 3 (set! read (cons 'first-clause read)))
             (range: (when (equal? line 
                                   "EINE ZEILE GRO/3..."))
                     (when (equal? line
                                   "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
                     (set! read (cons 'second-clause read)))
             (range: (when (equal? line
                                   "expr-test zu prüfen:"))
                     4
                     (set! read (cons 'third-clause read))))))
    (test-equal (list 'second-clause 'second-clause 'third-clause 'second-clause 
                      'third-clause 'first-clause 'first-clause)
      read)))

;; TODO: only ok <test>s ... s.u.
(define-test-case awk-tests range-w-begin-w-end ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
        (lambda (in-port)
          (awk (read-line in-port) (line) counter ()
               (:range: 1 3 (set! read (cons 'first-clause read)))
               (:range: (when (equal? line 
                                      "EINE ZEILE GRO/3..."))
                        (when (equal? line
                                      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
                        (set! read (cons 'second-clause read)))
               (:range: (when (equal? line
                                      "expr-test zu prüfen:"))
                        4
                        (set! read (cons 'third-clause read))))))
    (test-equal (list 'second-clause 'second-clause 'third-clause 'second-clause 'third-clause 
                  'second-clause 'first-clause 'third-clause 'first-clause 'first-clause)
      read)))

(define-test-case awk-tests else ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) ()
             (1 (set! read (cons 'first-clause read)))
             (else (set! read (cons 'second-clause read)))
             (4 (set! read (cons 'third-clause read)))
             (5 (set! read (cons 'fourth-clause read)))
             (else (set! read (cons 'fifth-clause read))))))
    (test-equal (list 'fifth-clause 'second-clause 'fourth-clause 'second-clause 'third-clause  
                      'second-clause 'fifth-clause 'second-clause 'fifth-clause 'second-clause 
                      'fifth-clause 'first-clause)
      read)))

(define-test-case awk-tests test=>expr ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ()
             (counter => (lambda (c)
                           (set! read (cons c read))))
             (#f  => (lambda (c)
                       (set! read (cons c read)))))))
    (test-equal (list 6 5 4 3 2 1) read)))

;; This one also happens to test proper scoping of the state
;; variables.
(define-test-case awk-tests test=>expr/rx ()
  (test-equal '(arrow-clause-test EINE "klein" "mit zeichen" plusplus)
    (call-with-string-input-port (some-test-lines "arrow-clause-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) ((result '()))
             ("arrow-clause-test" (cons 'arrow-clause-test result))
             ((: bos "eine zeile " ($ (+ (or alphabetic space))))
              => (lambda (match)
                   (cons (irregex-match-substring match 1) result)))
             ("EINE" (cons 'EINE result))
             ((+ "+") (cons 'plusplus result))
             (after
              (reverse result)))))))

(define-test-case awk-tests after ()
  (let ((read '()))
    (set! read
          (call-with-string-input-port (some-test-lines "expr-test")
            (lambda (in-port)
              (awk (read-line in-port) (line) ()
                   (1 (set! read 1))
                   (2 (set! read 2))
                   (after 'return)))))
    (test-equal 'return read)))

(define-test-case awk-tests var-decl ()
  (let ((read 0))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk (read-line in-port) (line) counter ((i 0)
                                                 (x 2)
                                                 (y 3))
             (1 (values (+ x y) x y))
             (2 (values i (+ i y) y))
             (3 (values (* i 2) x y))
             (4 (values (- i y) x y))
             (5 (values (* i x) x y))
             (6 (set! read i)
                (values i x y)))))
    (= read 56)))


(define-test-case awk-tests multiple-return-values-of-next-record ()
  (let ((read '()))
    (call-with-string-input-port (some-test-lines "expr-test")
      (lambda (in-port)
        (awk ((lambda ()
                (values (read-line in-port)1 2 'a 'b))) (line x y a b) counter ()
                (1 (set! read (cons x read)))
                (2 (set! read (cons y read)))
                (3 (set! read (cons a read)))
                (4 (set! read (cons b read))))))
    (test-equal (list 'b 'a 2 1)
      read)))

(set-test-debug-errors?! #t)
(run-test-suite awk-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
