;;@ Return an association list that corresponds to @1.
(define (table->alist table)
  (let ((alist '()))
    (table-walk table
                (lambda (key value)
                  (set! alist (cons (cons key value) alist))))
    alist))

(define (table-fold proc init table)
  (let ((result init))
    (table-walk (lambda (key value)
                  (set! result (proc key value result)))
                table)
    result))

(define (default-failure-thunk) #f)

;; arch-tag: ebb30766-d8c9-4468-8cb5-a3ceb5c4a592
