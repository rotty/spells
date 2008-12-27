;;; compat.ypsilon.sls --- filesys compat library for Ypsilon

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

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

          copy-file)
  (import (rnrs base)
          (prefix (rnrs files) rnrs:)
          (spells receive)
          (spells pathname)
          (spells filesys copy-file)
          (prefix (core files) yp:)
          (prefix (only (core primitives)
                        create-directory)
                  yp:))

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
  
  (define file-readable? (todo-proc 'file-readable?))
  (define file-writable? (todo-proc 'file-writable?))
  (define file-executable? (todo-proc 'file-executable?))
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


  (define working-directory (todo-proc 'working-directory))
  (define change-working-directory (todo-proc 'change-working-directory))

  (define-syntax with-working-directory
    (syntax-rules ()
      ((with-working-directory dir body ...)
       (let ((wd (working-directory)))
         (dynamic-wind
           (lambda () (change-working-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (change-working-directory wd)))))))
  
  )
