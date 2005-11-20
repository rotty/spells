(#%require (only mzscheme read-bytes write-bytes)
           (rename mzscheme mz:write-string write-string))

(define (read-block block start count port)
  (if (string? block)
      (read-string! block port start (+ start count))
      (read-bytes! block port start (+ start count))))

(define (write-block block start count port)
  (if (string? block)
      (mz:write-string block port start (+ start count))
      (write-bytes block port start (+ start count))))

(define (write-string string port)
  (mz:write-string string port))

;; arch-tag: a3459c3d-66d7-485d-b4fe-d2ca7f76dd6e
