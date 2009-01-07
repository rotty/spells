#!r6rs

;;@ Procedure annotations.
(library (spells procedure-annotations)
  (export annotate-procedure
          procedure-annotation)
  (import (rnrs base)
          (spells define-values))

  ;; Naive, portable implementation
  (define-values (annotate-procedure procedure-annotation)
    (let ((tag (list 'procedure-annotation)))
      (values
       (lambda (proc value)
         (lambda args
           (if (and (not (null? args))
                    (null? (cdr args))
                    (eq? (car args) tag))
               value
               (apply proc args))))
       (lambda (proc)
         (proc tag))))))
