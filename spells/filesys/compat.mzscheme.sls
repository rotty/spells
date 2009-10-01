;;; compat.mzscheme.sls --- filesys compat library for MzScheme.

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

          directory-stream?
          open-directory-stream
          close-directory-stream
          read-directory-stream
          
          working-directory
          with-working-directory

          library-search-paths)
  (import (except (rnrs) file-exists? delete-file)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)

          (prefix (only (mzscheme)
                        delete-directory
                        file-exists?
                        directory-exists?
                        link-exists?
                        make-directory
                        delete-file
                        rename-file-or-directory
                        file-or-directory-permissions
                        file-or-directory-modify-seconds
                        file-size
                        directory-list
                        path->string
                        current-directory
                        current-library-collection-paths

                        car cdr memq)
                  mz:)
          (only (scheme)
                with-handlers
                exn:fail:filesystem?
                exn-message)
          (only (scheme mpair) list->mlist))

(define ->fn ->namestring)

(define (file-exists? pathname)
  (let ((f (->fn pathname)))
    (or (mz:file-exists? f) (mz:directory-exists? f)
        (mz:link-exists? f))))

(define (create-directory pathname)
  (mz:make-directory (->fn pathname)))

(define (create-symbolic-link link-pathname linked-pathname)
  (error 'create-symbolic-link "not implemented on mzscheme"))

(define (create-hard-link link-pathname linked-pathname)
  (error 'create-hard-link "not implemented on mzscheme"))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (mz:delete-directory (->fn pathname))
          (mz:delete-file (->fn pathname)))))

(define (rename-file source-pathname target-pathname)
  (mz:rename-file-or-directory (->fn source-pathname) (->fn target-pathname) #t))

(define (file-regular? pathname)
  (mz:file-exists? (->fn pathname)))

(define (file-symbolic-link? pathname)
  (mz:link-exists? (->fn pathname)))

(define (file-directory? pathname)
  (mz:directory-exists? (->fn pathname)))

(define (make-file-check permission who)
  (lambda (pathname)
    (let ((fname (->fn pathname)))
      (with-handlers
        ((exn:fail:filesystem?
          (lambda (e)
            (raise (condition
                    (make-error)
                    (make-who-condition who)
                    (make-i/o-file-does-not-exist-error fname)
                    (make-message-condition (exn-message e)))))))
        (and (mz:memq permission (mz:file-or-directory-permissions fname))
             #t)))))

(define-syntax define-file-check
  (syntax-rules ()
    ((_ id pred)
     (define id (make-file-check pred 'id)))))

(define-file-check file-readable? 'read)
(define-file-check file-writable? 'write)
(define-file-check file-executable? 'execute)

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (mz:file-or-directory-modify-seconds (->fn pathname))))

(define (file-size-in-bytes pathname)
  (mz:file-size (->fn pathname)))

;; Emulate the stream with a list. TODO: file a wishlist bug on PLT to
;; support a stream API.
(define-record-type directory-stream
  (fields (mutable entries)))

(define (open-directory-stream pathname)
  (make-directory-stream
   (map mz:path->string (list->mlist (mz:directory-list (->fn pathname))))))

(define (close-directory-stream stream)
  (directory-stream-entries-set! stream #f))

(define (read-directory-stream stream)
  (let ((entries (directory-stream-entries stream)))
    (if (null? entries)
        #f
        (let ((filename (car entries)))
          (directory-stream-entries-set! stream (cdr entries))
          (if (or (string=? "." filename)
                  (string=? ".." filename))
              (read-directory-stream stream)
              filename)))))
  
(define (working-directory)
  (->pathname (mz:current-directory)))

(define (with-working-directory dir thunk)
  (let ((wd (mz:current-directory)))
    (dynamic-wind
      (lambda () (mz:current-directory
                  (->fn (pathname-as-directory (->pathname dir)))))
      thunk
      (lambda () (mz:current-directory wd)))))

(define (library-search-paths)
  (list->mlist (mz:current-library-collection-paths)))

)
