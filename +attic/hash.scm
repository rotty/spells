;;@ Returns a different number after each garbage collection.
(define (gc-stamp) (proc-to-be-defined))

;;@ Returns a hash value based on the address of @1. This hash value
;; does not have to be constant over garbage collection. See
;; @code{gc-stamp}.
(define (descriptor-hash object) (proc-to-be-defined))

;; arch-tag: 941e9be7-1096-412a-9386-87aeaed13f4e
