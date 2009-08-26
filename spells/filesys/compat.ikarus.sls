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
          (rnrs exceptions)
          (rnrs io ports)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (ikarus) ik:))

(define ->fn ->namestring)

(define (file-exists? pathname)
  (ik:file-exists? (->fn pathname)))

(define (create-directory pathname)
  (ik:make-directory (->fn pathname)))

(define (delete-file pathname)
  (let ((fname (->fn pathname)))
    (if (ik:file-exists? fname)
        (if (ik:file-directory? fname)
            (ik:delete-directory fname)
            (ik:delete-file fname)))))

(define (rename-file source-pathname target-pathname)
  (ik:rename-file (->fn source-pathname) (->fn target-pathname)))

(define (create-hard-link old-pathname new-pathname)
  (ik:make-hard-link (->fn old-pathname) (->fn new-pathname)))

(define (create-symbolic-link old-pathname new-pathname)
  (ik:make-symbolic-link (->fn old-pathname) (->fn new-pathname)))

(define (file-regular? pathname)
  (ik:file-regular? (->fn pathname)))

(define (file-symbolic-link? pathname)
  (ik:file-symbolic-link? (->fn pathname)))

(define (file-directory? pathname)
  (ik:file-directory? (->fn pathname)))

(define (make-file-check pred who)
  (lambda (pathname)
    (let ((fname (->fn pathname)))
      ;; This re-raising is there to produce an exception a more
      ;; specific type; see
      ;; <https://bugs.launchpad.net/ikarus/+bug/405944>.
      (guard (c ((i/o-filename-error? c)
                 (raise (condition
                         (make-error)
                         (make-who-condition who)
                         (make-i/o-file-does-not-exist-error fname)))))
        (pred fname)))))

(define-syntax define-file-check
  (syntax-rules ()
    ((_ id pred)
     (define id (make-file-check pred 'id)))))

(define-file-check file-readable? ik:file-readable?)
(define-file-check file-writable? ik:file-writable?)
(define-file-check file-executable? ik:file-executable?)

(define (file-modification-time pathname)
  (let ((nsecs (ik:file-mtime (->fn pathname))))
    (posix-timestamp->time-utc (div nsecs #e1e9) (mod nsecs #e1e9))))

(define (file-size-in-bytes pathname)
  (ik:file-size (->fn pathname)))

(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (let ((dirname (pathname-as-directory pathname)))
    (define (full-pathname entry)
      (pathname-with-file dirname (pathname-file (->pathname entry))))
    (let loop ((entries (ik:directory-list (->fn dirname))) (seeds seeds))
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
  (->pathname (ik:current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (ik:current-directory)))
    (dynamic-wind
      (lambda () (ik:current-directory
                  (->fn (pathname-as-directory (->pathname dir)))))
      thunk
      (lambda () (ik:current-directory wd)))))

(define library-search-paths ik:library-path)

)
