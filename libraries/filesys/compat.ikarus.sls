;;; compat.ikarus.sls --- filesys compat library for Ikarus.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


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

          library-search-paths)
  (import (rnrs base)
          (rnrs conditions)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (ikarus) ik:))

(define x->f x->namestring)

(define (file-exists? pathname)
  (ik:file-exists? (x->f pathname)))

(define (create-directory pathname)
  (ik:make-directory (x->f pathname)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (ik:delete-directory (x->f pathname))
          (ik:delete-file (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (ik:rename-file (x->f source-pathname) (x->f target-pathname)))

(define (create-hard-link old-pathname new-pathname)
  (ik:make-hard-link (x->f old-pathname) (x->f new-pathname)))

(define (create-symbolic-link old-pathname new-pathname)
  (ik:make-symbolic-link (x->f old-pathname) (x->f new-pathname)))

(define (file-regular? pathname)
  (ik:file-regular? (x->f pathname)))

(define (file-symbolic-link? pathname)
  (ik:file-symbolic-link? (x->f pathname)))

(define (file-directory? pathname)
  (ik:file-directory? (x->f pathname)))

(define (file-readable? pathname)
  (ik:file-readable? (x->f pathname)))
(define (file-writable? pathname)
  (ik:file-writable? (x->f pathname)))
(define (file-executable? pathname)
  (ik:file-executable? (x->f pathname)))

(define (file-modification-time pathname)
  (let ((nsecs (ik:file-mtime (x->f pathname))))
    (posix-timestamp->time-utc (div nsecs #e1e9) (mod nsecs #e1e9))))

(define (file-size-in-bytes pathname)
  (ik:file-size (x->f pathname)))

(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (let ((dirname (pathname-as-directory pathname)))
    (define (full-pathname entry)
      (pathname-with-file dirname (pathname-file (x->pathname entry))))
    (let loop ((entries (ik:directory-list (x->f dirname))) (seeds seeds))
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
                         (apply values new-seeds))))))))))

(define (working-directory)
  (x->pathname (ik:current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (ik:current-directory)))
    (dynamic-wind
      (lambda () (ik:current-directory
                  (x->f (pathname-as-directory (x->pathname dir)))))
      thunk
      (lambda () (ik:current-directory wd)))))

(define library-search-paths ik:library-path)

)
