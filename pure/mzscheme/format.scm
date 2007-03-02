(scmxlate-ignore-define ascii-tab unspecified)

(#%require (rename mzscheme mzscheme-format format))

(define (format port format-string . args)
  (let ((result (apply mzscheme-format format-string args)))
    (cond ((eq? port #t)
           (display result))
          (port
           (display result port))
          (else result))))

;; arch-tag: 0774343a-a9e7-4eb9-b0bf-f52af0e7d9f2
