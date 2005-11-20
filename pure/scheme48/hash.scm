(define (gc-stamp)
  (memory-status (enum memory-status-option gc-count) 0))

(define (descriptor-hash object)
  (memory-status (enum memory-status-option pointer-hash) object))

;; arch-tag: 0ab52de1-9a63-4cb3-8422-65b4c4e335d9
