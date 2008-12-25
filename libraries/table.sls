#!r6rs

;;@ Simple hash tables.
(library (spells table)
  (export make-table
          table?
          table-ref
          table-set!
          table-walk
          table-fold
          table->alist)
  (import (rnrs base)
          (spells table compat))  

  ;;@ Return an association list that corresponds to @1.
  (define (table->alist table)
    (let ((alist '()))
      (table-walk table
                  (lambda (key value)
                    (set! alist (cons (cons key value) alist))))
      alist))

  (define (table-fold proc init table)
    (let ((result init))
      (table-walk table
                  (lambda (key value)
                    (set! result (proc key value result))))
      result))

  (define (default-failure-thunk) #f))

;; arch-tag: ebb30766-d8c9-4468-8cb5-a3ceb5c4a592
