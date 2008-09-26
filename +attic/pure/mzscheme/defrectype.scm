(require (lib "defmacro.ss"))
(require-for-syntax spells.define-record-type*-expander)

(define-macro (define-record-type* . forms)
  (expand-define-record-type* (cons 'define-record-type* forms)
                              (lambda (x) x)
                              eq?))

(define-macro (define-functional-fields . forms)
  (expand-define-functional-fields (cons 'define-record-type* forms)
                                   (lambda (x) x)
                                   eq?))

;; How to implement this?
(define-syntax define-record-discloser
  (syntax-rules ()
    ((define-record-discloser type proc)
     #t)))
