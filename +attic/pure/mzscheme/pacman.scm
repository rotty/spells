(#%require (only mzscheme
                 make-namespace current-namespace namespace-require dynamic-require
                 namespace-attach-module
                 parameterize make-parameter)
           (lib "list.ss" "srfi" "1")
           (lib "13.ss" "srfi")
           (lib "14.ss" "srfi"))

(define (string-split str chr)
  (string-tokenize str (char-set-complement (char-set chr))))

(define (open->require name)
  (if (list? name)
      name
      (let* ((ns (symbol->string name))
             (srfi? (string-prefix? "srfi-" ns)))
        (cond ((eq? name 'scheme)
               '(lib "lang.ss" "r5rs"))
              ((and srfi? (string=? ns "srfi-1"))
               '(lib "list.ss" "srfi" "1"))
              ((and srfi? (string=? ns "srfi-43"))
               '(lib "vector-lib.ss" "srfi" "43"))
              (else
               (let* ((vals (if srfi?
                                (reverse (string-split ns #\-))
                                (list ns)))
                      (fst (string-append (car vals)
                                          (if srfi? ".ss" ".scm"))))
                 (if (null? (cdr vals))
                     (string->symbol (car vals))
                     (append (list 'lib fst) (reverse (cdr vals))))))))))


(define system-loader (make-parameter (lambda (system-name) #f)))

(define (load/package filespec pkg)
  (parameterize ((current-namespace pkg))
    (load filespec)))

(define (make-package name open)
  (let ((package (make-namespace 'empty))
        (reqs (filter-map open->require open))
        (ns (current-namespace)))
    (for-each (lambda (req) (dynamic-require req #f)) reqs)
    (parameterize ((current-namespace package))
      (for-each (lambda (module)
                  (namespace-attach-module ns module)
                  (namespace-require module))
                reqs))
    package))
