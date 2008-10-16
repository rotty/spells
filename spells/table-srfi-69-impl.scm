;;; Implementation of the (spells table compat) interface on top of SRFI-69

(define make-table
  (case-lambda
    (() (make-hash-table equal?))
    ((type)
     (case type
       ((eq) (make-hash-table eq?))
       ((eqv) (make-hash-table eqv?))
       ((equal) (make-hash-table equal?))
       (else (error 'make-table "invalid hash table type" type))))))

(define (table? thing)
  (hash-table? thing))

(define (table-ref table key . failure-thunk)
  (let ((thunk (and (pair? failure-thunk) (car failure-thunk))))
    (if thunk
        (hash-table-ref table key thunk)
        (hash-table-ref table key (lambda () #f)))))

(define (table-set! table key value)
  (hash-table-set! table key value))

(define (table-walk table proc)
  (hash-table-walk table proc))
