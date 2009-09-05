;;; aux.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells include helpers)
  (export include-file/aux)
  (import (rnrs)
          (spells tracing)
          (only (spells filesys) find-file library-search-paths)
          (only (spells pathname) ->namestring))

  (define (error/conditions who msg irrts . cndts)
    (raise
      (apply condition
             (make-error)
             (make-who-condition who)
             (make-message-condition msg)
             (make-irritants-condition irrts)
             cndts)))
  
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

  (define (include-file/aux who ctxt path transformer)
    (let* ((relpath (filespec->path path))
           (pathname (find-file relpath (library-search-paths))))
      (unless pathname
        (error 'include-file "cannot find file in search paths"
               relpath
               (library-search-paths)))
      (let ((filename (->namestring pathname)))
        (with-exception-handler
            (lambda (ex)
              (error/conditions who
                                "error while trying to include"
                                (list filename)
                                (if (condition? ex)
                                    ex
                                    (make-irritants-condition (list ex)))))
          (lambda ()
            (call-with-input-file filename
              (lambda (port)
                (let loop ((x (read port)) (forms '()))
                  (if (eof-object? x)
                      (cons #'begin (datum->syntax ctxt (reverse forms)))
                      (loop (read port) (cons (transformer x) forms)))))))))))

)
