;;; run.sls --- Utilities for running testcases

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Utilities for running testcases.
(library (spells testing run)
  (export run-tests
          run-tests-in-directory
          run-tests-in-file
          main)
  (import (except (rnrs base) string-copy string->list string-for-each)
          (rnrs eval)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs exceptions)
          (rnrs programs)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (srfi :8 receive)
          (spells misc)
          (srfi :39 parameters)
          (spells match)
          (spells filesys)
          (spells pathname)
          (spells condition)
          (only (spells testing) with-test-verbosity)
          (spells testing run-env))

  ;;@ Run specified tests.
  ;;
  ;; @1 must be a list whose elements can be either directories or
  ;; single file names: @code{run-tests-in-directory} or
  ;; @code{run-tests-in-file} is applied accordingly. If empty is
  ;; provided, the current directory is used.
  (define (run-tests tests env)
    (for-each (lambda (f)
                (cond ((file-directory? f) (run-tests-in-directory f env))
                      ((file-regular? f) (run-tests-in-file f env))
                      (else
                       (display (list "skipping non-existing" f))
                       (newline))))
              tests))

  ;;@ Call @code{run-tests-in-file} for each file in the given
  ;; directory. If the directory contains a file named @file{tests.scm},
  ;; the list of files is read from it.
  (define (run-tests-in-directory dir env)
    (let ((listing (make-pathname #f dir (make-file "tests" "scm"))))
      (let ((files (if (file-regular? listing)
                       (map (lambda (x)
                              (pathname-with-file dir (cond ((string? x) x)
                                                            (else (car x)))))
                            (with-input-from-file (x->namestring listing) read))
                       (directory-fold dir cons '()))))
        (for-each (lambda (f)
                    (run-tests-in-file f env))
                  files))))

  (define (run-tests-in-file file env)
    (let ((filename (x->namestring file)))
      (display "\n;")
      (display (list "Loading " filename "... "))
      (newline)
      (receive results
               (call-with-input-file filename
                 (lambda (port)
                   (let loop ((forms '()))
                     (let ((form (read port)))
                       (if (eof-object? form)
                           (eval `(let () ,@(reverse forms)) env)
                           (loop (cons form forms)))))))
        (display ";")
        (display (list "..." filename "done"))
        (newline)
        (apply values results))))

  (define package-name->import-spec
    (let ((all-but-dot (char-set-complement (char-set #\.))))
      (lambda (spec)
        (if (symbol? spec)
            (map string->symbol (string-tokenize (symbol->string spec) all-but-dot))
            spec))))

  (define (construct-test-environment imports)
    (guard (c (#t (display ";#(Error constructing environment: ")
                  (newline)
                  (display-condition c)
                  (display ")")
                  (newline)
                  #f))
      (apply environment
             (append
              '((except (rnrs base) error string-copy string-for-each string->list)
                (rnrs io simple)
                (spells testing)
                (spells testing run-env))
              imports))))

  ;; test spec grammar:
  ;;
  ;; <test spec> -> (<clause> ...)
  ;; <clause> -> (files (<file spec> <required library>) ...)
  ;; <file spec> -> <filename>
  ;;                (code <scheme expr> ...) <filename>
  (define (eval-test-spec pathname test-spec tests)
    (for-each
     (lambda (spec)
       (case (car spec)
         ((files)
          (for-each
           (lambda (clause)
             (eval-test-clause clause pathname tests))
           (cdr spec)))))
     test-spec))


  (define (eval-test-clause clause pathname tests)
    (define (run-test code filename pkgs)
      (when (or (null? tests) (member filename tests))
        (let ((env (construct-test-environment
                    (map package-name->import-spec pkgs))))
          (when env
            (parameterize ((test-environment env))
              (with-test-verbosity 'quiet
                (lambda ()
                  (unless (null? code)
                    (eval `(let () ,@code) env))
                  (run-tests
                   (list (pathname-with-file pathname filename))
                   env))))))))
    (match clause
      ((('code . code) filename . pkgs)
       (run-test code filename pkgs))
      ((filename . pkgs)
       (run-test '() filename pkgs))))
  
  (define (main args)
    (for-each (lambda (tests-file)
                (call-with-input-file tests-file
                  (lambda (port)
                    (let ((test-spec (read port))
                          (pathname (x->pathname tests-file)))
                      (parameterize
                          ((this-directory (directory-namestring pathname)))
                        (eval-test-spec pathname test-spec '()))))))
              (cdr args))))

;; Local Variables:
;; scheme-indent-styles: ((match 1) (parameterize 1))
;; End:
