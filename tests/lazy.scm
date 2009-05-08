;=========================================================================
; TESTS AND BENCHMARKS:
;=========================================================================

(define-test-suite lazy-tests
  "SRFI 45: Primitives for Expressing Iterative Lazy Algorithms")

(define-syntax test-output
  (syntax-rules ()
    ((_ expected proc)
     (test-equal expected
       (call-with-string-output-port proc)))))

(define-test-case lazy-tests memo1 ()
  (test-output "hello"
    (lambda (port)
      (define s (delay (begin (display 'hello port) 1)))
      (test-equal 1 (force s))
      (force s))))

(define-test-case lazy-tests memo2 ()
  (test-output "bonjour"
    (lambda (port)
      (let ((s (delay (begin (display 'bonjour port) 2))))
        (test-equal 4 (+ (force s) (force s)))))))

(define-test-case lazy-tests memo3 ()
  (test-output "hi"
    (lambda (port)
      (define r (delay (begin (display 'hi port) 1)))
      (define s (lazy r))
      (define t (lazy s))
      (test-equal 1 (force t))
      (test-equal 1 (force r)))))

;=========================================================================
; Memoization test 4: Stream memoization

(define (stream-drop s index)
  (lazy
   (if (zero? index)
       s
       (stream-drop (cdr (force s)) (- index 1)))))

(define (ones port)
  (delay (begin
           (display 'ho port)
           (cons 1 (ones port)))))

(define-test-case lazy-tests memo4 ()
  (test-output "hohohohoho"
    (lambda (port)
      (define s (ones port))
      (test-equal 1
        (car (force (stream-drop s 4))))
      (test-equal 1
        (car (force (stream-drop s 4)))))))

;=========================================================================
; Reentrancy test 1: from R5RS

(define-test-case lazy-tests reenter1 ()
  (letrec ((count 0)
           (p (delay (begin (set! count (+ count 1))
                            (if (> count x)
                                count
                                (force p)))))
           (x 5))
    (test-equal 6 (force p))
    (set! x 10)
    (test-equal 6 (force p))))

;=========================================================================
; Reentrancy test 2: from SRFI 40

(define-test-case lazy-tests reenter2 ()
  (letrec ((f (let ((first? #t))
                (delay
                  (if first?
                      (begin
                        (set! first? #f)
                        (force f))
                      'second)))))
    (test-equal 'second (force f))))

;=========================================================================
; Reentrancy test 3: due to John Shutt

(define-test-case lazy-tests reenter3 ()
  (let* ((q (let ((count 5))
              (define (get-count) count)
              (define p (delay (if (<= count 0)
                                   count
                                   (begin (set! count (- count 1))
                                          (force p)
                                          (set! count (+ count 2))
                                          count))))
              (list get-count p)))
         (get-count (car q))
         (p (cadr q)))

    (test-equal 5 (get-count))
    (test-equal 0 (force p))
    (test-equal 10 (get-count))))

#|
;=========================================================================
; Test leaks:  All the leak tests should run in bounded space.

;=========================================================================
; Leak test 1: Infinite loop in bounded space.

(define (loop) (lazy (loop)))
;(force (loop))                               ;==> bounded space

;=========================================================================
; Leak test 2: Pending memos should not accumulate
;              in shared structures.

(define s (loop))
;(force s)                                    ;==> bounded space

;=========================================================================
; Leak test 3: Safely traversing infinite stream.

(define (from n)
  (delay (cons n (from (+ n 1)))))

(define (traverse s)
  (lazy (traverse (cdr (force s)))))

;(force (traverse (from 0)))                  ;==> bounded space

;=========================================================================
; Leak test 4: Safely traversing infinite stream
;              while pointer to head of result exists.

(define s (traverse (from 0)))
;(force s)                                    ;==> bounded space

;=========================================================================
; Convenient list deconstructor used below.

(define-syntax match
  (syntax-rules ()
    ((match exp
       (()      exp1)
       ((h . t) exp2))
     (let ((lst exp))
       (cond ((null? lst) exp1)
             ((pair? lst) (let ((h (car lst))
                                (t (cdr lst)))
                            exp2))
             (else 'match-error))))))

;========================================================================
; Leak test 5: Naive stream-filter should run in bounded space.
;              Simplest case.

(define (stream-filter p? s)
  (lazy (match (force s)
          (()      (delay '()))
          ((h . t) (if (p? h)
                       (delay (cons h (stream-filter p? t)))
                       (stream-filter p? t))))))

;(force (stream-filter (lambda (n) (= n 10000000000))
;                      (from 0)))
                                             ;==> bounded space

;========================================================================
; Leak test 6: Another long traversal should run in bounded space.

; The stream-ref procedure below does not strictly need to be lazy.
; It is defined lazy for the purpose of testing safe compostion of
; lazy procedures in the times3 benchmark below (previous
; candidate solutions had failed this).

(define (stream-ref s index)
  (lazy
   (match (force s)
     (()      'error)
     ((h . t) (if (zero? index)
                  (delay h)
                  (stream-ref t (- index 1)))))))

; Check that evenness is correctly implemented - should terminate:

(force (stream-ref (stream-filter zero? (from 0))
                   0))                              ;==> 0

(define s (stream-ref (from 0) 100000000))
;(force s)                                          ;==> bounded space

;======================================================================
; Leak test 7: Infamous example from SRFI 40.

(define (times3 n)
  (stream-ref (stream-filter
               (lambda (x) (zero? (modulo x n)))
               (from 0))
              3))

(force (times3 7))
;(force (times3 100000000))                        ;==> bounded space

|#

(run-test-suite lazy-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-output 1))
;; End:
