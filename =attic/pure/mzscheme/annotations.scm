(#%require (only mzscheme define-values make-struct-type))

(define-values (struct:ap annotate-procedure annotated-procedure? ap-ref ap-set!)
  (make-struct-type 'annotated-procedure #f 2 0 #f '() #f 0))

(define (procedure-annotation proc)
  (and (annotated-procedure? proc)
       (ap-ref proc 1)))
