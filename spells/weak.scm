;;@ Create a weak pointer to @1.
(define (make-weak-pointer obj) (proc-to-be-defined))

;;@ Return the value contained in @1, or @code{#f} if the value was
;; garbage collected.
(define (weak-pointer-ref weak-pointer) (proc-to-be-defined))

;;@ Returns @code{#t} if @1 is a weak pointer. Note that disjointness
;; of weak pointers is not guaranteed.
(define (weak-pointer? thing) (proc-to-be-defined))
