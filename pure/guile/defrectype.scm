(define-macro (define-record-type* . forms)
  (expand-define-record-type* (cons 'define-record-type* forms)
                              (lambda (x) x)
                              eq?))

(define-macro (define-functional-fields . forms)
  (expand-define-functional-fields (cons 'define-record-type* forms)
                                   (lambda (x) x)
                                   eq?))

;; How to implement this?
(define (define-record-discloser type proc)
  #t)
