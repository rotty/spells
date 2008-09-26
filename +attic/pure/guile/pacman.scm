(use-modules (spells parameter)
             (srfi srfi-1))

(define (guile-module-name symbol)
  (map string->symbol (string-split (symbol->string symbol) #\.)))

(define (open->imports name)
  (cond ((eq? name 'scheme) #f)
        (else
         (let* ((ns (symbol->string name))
                (srfi? (string-prefix? "srfi-" ns)))
           (cond (srfi? `(srfi ,name))
                 (else (guile-module-name name)))))))

(define system-loader (make-parameter (lambda (system-name) #f)))

(define (load/package filespec pkg)
  (save-module-excursion
   (lambda ()
     (set-current-module pkg)
     (load filespec))))

(define (make-package name open)
  (let ((ifaces (map resolve-interface (cons '(ice-9 r5rs) (filter-map open->imports open)))))
    (make-module 1021 ifaces)))
