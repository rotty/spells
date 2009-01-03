(define (annotate-procedure proc value)
  (set-procedure-property! proc '%annotation value)
  proc)

(define (procedure-annotation proc)
  (procedure-property proc '%annotation))
