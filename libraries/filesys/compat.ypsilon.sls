;;; compat.ypsilon.sls --- filesys compat library for Ypsilon.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs


(library (spells filesys compat)
  (export file-exists?
          create-directory
          create-symbolic-link
          create-hard-link
          delete-file
          rename-file

          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-fold*

          working-directory
          with-working-directory

          copy-file
          library-search-paths)
  (import (rnrs base)
          (prefix (rnrs files) rnrs:)
          (xitomatl srfi receive)
          (spells pathname)
          (spells filesys copy-file)
          (prefix (core files) yp:)
          (prefix (core primitives) yp:))

  (define x->f x->namestring)

  (define (todo-proc who)
    (lambda args
      (error who "please implement me!")))

  (define (file-exists? pathname)
    (rnrs:file-exists? (x->f pathname)))

  (define (create-directory pathname)
    (yp:create-directory (x->f pathname)))

  (define create-symbolic-link (todo-proc 'create-symbolic-link))
  (define create-hard-link (todo-proc 'create-hard-link))

  (define (delete-file pathname)
    (rnrs:delete-file (x->f pathname)))

  (define rename-file (todo-proc 'rename-file))

  (define (file-regular? pathname)
    (yp:file-regular? (x->f pathname)))
  (define (file-directory? pathname)
    (yp:file-directory? (x->f pathname)))
  (define (file-symbolic-link? pathname)
    (yp:file-symbolic-link? (x->f pathname)))

  (define (file-readable? pathname)
    (yp:file-readable? (x->f pathname)))

  (define (file-writable? pathname)
    (yp:file-writable? (x->f pathname)))

  (define (file-executable? pathname)
    (yp:file-executable? (x->f pathname)))

  (define file-modification-time (todo-proc 'file-modification-time))

  (define (file-size-in-bytes pathname)
    (yp:file-size-in-bytes (x->f pathname)))

  (define (dot-or-dotdot? f)
    (or (string=? "." f) (string=? ".." f)))

  (define (directory-fold* pathname combiner . seeds)
    (define (full-pathname entry)
      (pathname-with-file pathname (pathname-file (x->pathname entry))))
    (let loop ((entries (yp:directory-list (x->f pathname))) (seeds seeds))
      (if (null? entries)
          (apply values seeds)
          (let ((entry (car entries)))
            (cond ((dot-or-dotdot? entry)
                   (loop (cdr entries) seeds))
                  (else
                   (receive (continue? . new-seeds)
                            (apply combiner (full-pathname entry) seeds)
                     (if continue?
                         (loop (cdr entries) new-seeds)
                         (apply values new-seeds)))))))))


  (define (working-directory)
    (yp:current-directory))

  (define-syntax with-working-directory
    (syntax-rules ()
      ((with-working-directory dir body ...)
       (let ((wd (yp:current-directory)))
         (dynamic-wind
           (lambda () (yp:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (yp:current-directory wd)))))))

  
  (define library-search-paths yp:scheme-library-paths)
  
  )
