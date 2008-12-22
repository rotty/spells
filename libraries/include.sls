#!r6rs

(library (spells include)
  (export include-file include/resolve)
  (import (rnrs)
          (for (xitomatl include compat) expand)
          (for (only (xitomatl include) include/resolve include/lexical-context) run expand))

  ;; We should use (spells find-file) here, but that still depends on
  ;; us -- this can be fixed by elimating include-file usage from the
  ;; (spells find-file) dependencies
  (define-syntax include-file
    (lambda (stx)
      (define (string-join lst sep)
        (if (null? lst)
            ""
            (let loop ((result '()) (lst lst))
              (if (null? lst)
                  (apply string-append (cdr (reverse result)))
                  (loop (cons (car lst) (cons sep result))
                        (cdr lst))))))
      (define (filespec->path name)
        (cond ((string? name) name)
              ((symbol? name) (string-append (symbol->string name) ".scm"))
              ((pair? name) (string-append
                             (if (pair? (car name))
                                 (string-join (map symbol->string (car name)) "/")
                                 (symbol->string (car name)))
                             "/"
                             (symbol->string (cadr name))
                             ".scm"))
              (else name)))

      (syntax-case stx ()
        ((k <path>)
         (let ((relpath (filespec->path (syntax->datum #'<path>))))
           (let loop ((search (search-paths)))
             (if (null? search)
               (error 'include-file "cannot find file in search paths"
                      relpath
                      (search-paths))
               (let ((full (string-append (car search) "/" relpath)))
                 (if (file-exists? full)
                   #`(include/lexical-context k #,full)
                   (loop (cdr search))))))))))))
