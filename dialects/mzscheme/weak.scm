(#%require (only mzscheme make-weak-box weak-box-value weak-box?))

(define make-weak-pointer make-weak-box)
(define weak-pointer? weak-box?)
(define weak-pointer-ref weak-box-value)

;; arch-tag: 6da76447-2d6d-4861-824f-88d06f22d4a4
