#!r6rs

(library (spells include)
  (export include)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) run expand)
          (for (rnrs io simple) expand)
          (for (rnrs io ports) expand))
  
  (define-syntax include
    (lambda (x)
      (define (string-join lst sep)
        (if (null? lst)
            ""
            (let loop ((result '()) (lst lst))
              (if (null? lst)
                  (apply string-append (cdr (reverse result)))
                  (loop (cons (car lst) (cons sep result))
                        (cdr lst))))))
      (define (filespec->path name)
        (cond ((symbol? name) (string-append (symbol->string name) ".scm"))
              ((pair? name) (string-append
                             (if (pair? (car name))
                                 (string-join (map symbol->string (car name)) "/")
                                 (symbol->string (car name)))
                             "/"
                             (symbol->string (cadr name))
                             ".scm"))
              (else name)))
      (define read-file
        (lambda (fn k)
          (call-with-input-file fn
            (lambda (p)
              (let f ((x (get-datum p)))
                (if (eof-object? x)
                    '()
                    (cons (datum->syntax k x)
                          (f (get-datum p)))))))))
      (syntax-case x ()
        ((k filespec ...)
         (let ((forms
                (let loop ((filespecs (syntax->datum #'(filespec ...))) (forms-list '()))
                  (if (null? filespecs)
                      (apply append (reverse forms-list))
                      (loop (cdr filespecs)
                            (cons (read-file (filespec->path (car filespecs)) #'k)
                                  forms-list))))))
           (with-syntax (((exp ...) forms))
             #'(begin exp ...))))))))
