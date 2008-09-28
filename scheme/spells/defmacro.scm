(define-syntax define-macro
  (lambda (stx)
    (syntax-case stx ()
      ((_ (macro . args) . body)
       (syntax (define-macro macro (lambda args . body))))
      ((_ macro transformer)
       (syntax
        (define-syntax macro
          (lambda (stx2)
            (let ((v (syntax->datum stx2)))
              (datum->syntax
               ;; we need the *identifier* of the macro call
               ;; (there is probably a smarter way of extracting that ...)
               (syntax-case stx2 () ((name . more) (syntax name)))
               (apply transformer (cdr v)))))))))))
