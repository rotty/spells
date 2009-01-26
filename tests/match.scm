(define-test-suite match-tests
  "Alex Shinn's hygienic pattern matcher macro")

(define (test-failure:named-equality name
                                     expected-expression expected-datum
                                     actual-expression actual-datum)
  (test-failure (string-append name ": expression's value failed comparison")
                `(expected ,expected-expression => ,expected-datum)
                `(got ,actual-expression => ,actual-datum)))

(define-syntax test:equal
  (syntax-rules ()
    ((test:equal name actual-expression expected-expression)
     (let ((expected-datum expected-expression)
           (actual-datum actual-expression))
       (if (not (equal? expected-datum actual-datum))
           (test-failure:named-equality name
                                        'expected-expression expected-datum
                                        'actual-expression actual-datum))))))

(define-test-case match-tests basics ()
  (test:equal "any" (match 'any (_ 'ok)) 'ok)
  (test:equal "symbol" (match 'ok (x x)) 'ok)
  (test:equal "number" (match 28 (28 'ok)) 'ok)
  (test:equal "string" (match "good" ("bad" 'fail) ("good" 'ok)) 'ok)
  (test:equal "literal symbol" (match 'good ('bad 'fail) ('good 'ok)) 'ok)
  (test:equal "null" (match '() (() 'ok)) 'ok)
  (test:equal "pair" (match '(ok) ((x) x)) 'ok)
  (test:equal "vector" (match '#(ok) (#(x) x)) 'ok)
  (test:equal "any doubled" (match '(1 2) ((__ __) 'ok)) 'ok)
  (test:equal "and empty" (match '(o k) ((and) 'ok)) 'ok)
  (test:equal "and single" (match 'ok ((and x) x)) 'ok)
  (test:equal "and double" (match 'ok ((and (? symbol?) y) 'ok)) 'ok)
  (test:equal "or empty" (match '(o k) ((or) 'fail) (else 'ok)) 'ok)
  (test:equal "or single" (match 'ok ((or x) 'ok)) 'ok)
  (test:equal "or double" (match 'ok ((or (? symbol? y) y) y)) 'ok)
  (test:equal "not" (match 28 ((not (a . b)) 'ok)) 'ok)
  (test:equal "pred" (match 28 ((? number?) 'ok)) 'ok)
  (test:equal "named pred" (match 28 ((? number? x) (+ x 1))) 29)

  (test:equal "duplicate symbols pass" (match '(ok . ok) ((x . x) x)) 'ok)
  (test:equal "duplicate symbols fail" (match '(ok . bad) ((x . x) 'bad) (else 'ok)) 'ok)
  (test:equal "duplicate symbols samth" (match '(ok . ok) ((x . 'bad) x) (('ok . x) x)) 'ok))

(define-test-case match-tests ellipses ()
  (test:equal "ellipses"
              (match '((a . 1) (b . 2) (c . 3))
                (((x . y) ___) (list x y)))
              '((a b c) (1 2 3)))

  (test:equal "real ellipses"
              (match '((a . 1) (b . 2) (c . 3))
                (((x . y) ...) (list x y)))
              '((a b c) (1 2 3)))

  (test:equal "vector ellipses"
              (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
                (#(a b c (hd . tl) ...) (list a b c hd tl)))
              '(1 2 3 (a b c) (1 2 3)))

  (test:equal "pred ellipses"
              (match '(1 2 3)
                (((? odd? n) ___) n)
                (((? number? n) ___) n))
              '(1 2 3)))

(define-test-case match-tests misc ()
  (test:equal "failure continuation"
              (match '(1 2)
                ((a . b) (=> next) (if (even? a) 'fail (next)))
                ((a . b) 'ok))
              'ok)

  (test:equal "let"
              (match-let ((x 'ok) (y '(o k)))
                         y)
              '(o k))

  (test:equal "let*"
              (match-let* ((x 'f) (y 'o) ((z w) (list y x)))
                          (list x y z w))
              '(f o o f))

  (test:equal "getter car"
              (match '(1 . 2) (((get! a) . b) (list (a) b)))
              '(1 2))

  (test:equal "getter cdr"
              (match '(1 . 2) ((a . (get! b)) (list a (b))))
              '(1 2))

  (test:equal "getter vector"
              (match '#(1 2 3) (#((get! a) b c) (list (a) b c)))
              '(1 2 3))

  (test:equal "setter car"
              (let ((x '(1 . 2)))
                (match x (((set! a) . b) (a 3)))
                x)
              '(3 . 2))

  (test:equal "setter cdr"
              (let ((x '(1 . 2)))
                (match x ((a . (set! b)) (b 3)))
                x)
              '(1 . 3))

  (test:equal "setter vector"
              (let ((x '#(1 2 3)))
                (match x (#(a (set! b) c) (b 0)))
                x)
              '#(1 0 3)))

#|
(begin
(define-record point x y)

(test:equal "record"
            (match (make-point 123 456) (($ point x y) (list x y)))
            '(123 456))

(test:equal "record nested"
            (match (make-point 123 '(456 789)) (($ point x (y z)) (list x y z)))
            '(123 456 789))

(test:equal "record getter"
            (let ((p (make-point 123 456)))
              (match p (($ point x (get! y)) (list x (y)))))
            '(123 456))

(test:equal "record setter"
            (let ((p (make-point 123 456)))
              (match p (($ point x (set! y)) (y 789)))
              (list (point-x p) (point-y p)))
            '(123 789))
)
|#

(define-test-case match-tests tails ()
  (test:equal "single tail"
              (match '((a . 1) (b . 2) (c . 3))
                (((x . y) ... last) (list x y last)))
              '((a b) (1 2) (c . 3)))

  (test:equal "single tail 2"
              (match '((a . 1) (b . 2) 3)
                (((x . y) ... last) (list x y last)))
              '((a b) (1 2) 3))

  (test:equal "multiple tail"
              (match '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
                (((x . y) ... u v w) (list x y u v w)))
              '((a b) (1 2) (c . 3) (d . 4) (e . 5))))

(define-test-case match-tests special ()
  (test:equal "Riastradh quasiquote"
              (match '(1 2 3) (`(1 ,b ,c) (list b c)))
              '(2 3))

  (test:equal "sjamaan uri"
              (match '(#\% 1 2) ((#\% h1 h2) #t) (else #f))
              #t))

(run-test-suite match-tests)

