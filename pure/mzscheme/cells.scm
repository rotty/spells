(#%require (only mzscheme box box? unbox set-box!))

(define make-cell box)
(define cell? box?)
(define cell-ref unbox)
(define cell-set! set-box!)

;; arch-tag: 9757c6fa-e6d8-42cb-9497-c61ac7029622
