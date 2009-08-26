;;; include.sls --- Include scheme source code.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells include)
  (export include-file)
  (import (rnrs)
          (for (only (spells filesys) find-file library-search-paths)
               expand)
          (for (only (spells pathname) ->namestring)
               expand))

  (define-syntax include/lexical-context
    (lambda (stx)
      (define (error/conditions who msg irrts . cndts)
        (raise
         (apply condition
                (make-error)
                (make-who-condition who)
                (make-message-condition msg)
                (make-irritants-condition irrts)
                cndts)))
      (syntax-case stx ()
        ((_ ctxt filename)
         (and (identifier? #'ctxt)
              (or (let ((path (syntax->datum #'filename)))
                    (and (string? path)
                         (positive? (string-length path))))
                  (syntax-violation #f "not a path" stx #'filename)))
         (let ((fn (syntax->datum #'filename)))
           (with-exception-handler
             (lambda (ex)
               (error/conditions 'include/lexical-context
                 "error while trying to include" (list fn)
                 (if (condition? ex) ex (make-irritants-condition (list ex)))))
             (lambda ()
               (call-with-input-file fn
                 (lambda (fip)
                   (let loop ((x (read fip)) (a '()))
                     (if (eof-object? x)
                       (datum->syntax #'ctxt `(begin . ,(reverse a)))
                       (loop (read fip) (cons x a)))))))))))))

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
         (let* ((relpath (filespec->path (syntax->datum #'<path>)))
                (pathname (find-file relpath (library-search-paths))))
           (unless pathname
               (error 'include-file "cannot find file in search paths"
                      relpath
                      (library-search-paths)))
           #`(include/lexical-context k #,(->namestring pathname))))))))
