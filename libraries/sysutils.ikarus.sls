;;@ Miscellaneous procedures providing access to various bits of
;; information regarding the host running the scheme implementation.
(library (spells sysutils)
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          find-exec-path
          os-name
          os-node-name
          os-release-name
          os-version-name
          machine-name)
  (import (rnrs base)
          (xitomatl srfi receive)
          (spells find-file)
          (only (xitomatl srfi strings) string-tokenize string-index)
          (only (xitomatl srfi char-set) char-set-complement char-set)
          (only (ikarus)
                getenv
                host-info))

  
  (define (string-split str chr)
    (string-tokenize str (char-set-complement (char-set chr))))
  
  (define lookup-environment-variable getenv)

  (define (extend-process-environment alist)
    (error 'extend-process-environment "not implemented on ikarus"))

  (define (current-process-environment)
    (error 'current-process-environment "not implemented on ikarus"))
  
  (define (find-exec-path prog)
    (let ((paths (string-split (getenv "PATH") #\:)))
      (find-file prog paths)))

  (define (host-info-values)
    (let* ((hi (host-info))
           (first-dash (string-index hi #\-))
           (second-dash (and first-dash (string-index hi #\- (+ first-dash 1)))))
      (cond ((and first-dash second-dash)
             (values (substring hi 0 first-dash)
                     (substring hi (+ first-dash 1) second-dash)
                     (substring hi (+ second-dash 1) (string-length hi))))
            (first-dash
             (values (substring hi 0 first-dash)
                     #f
                     (substring hi (+ first-dash 1) (string-length hi))))
            (else
             (values #f #f hi)))))
  
  (define (os-name)
    (receive (cpu vendor os)
             (host-info-values)
      os))
  
  (define (os-node-name) "unknown")
  (define (os-release-name) "unknown")
  (define (os-version-name) "unknown")
  (define (machine-name) (host-info))

  
)
