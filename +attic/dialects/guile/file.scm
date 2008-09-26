;; guile-file.scm -- Guile-specific file.scm definitions
;; arch-tag: 0ce1bf3a-29fd-11d9-823b-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Oct 30, 2004 00:51

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Comentary:

;; Guile-specific definitions for file.scm

;;; Code:

;;; (Re)definitions:

(define file-basename basename)
(define file-dirname dirname)
(define (file? f) (and (file-exists? f) (not (directory? f))))
(define (file-is-readable? f) (access? f R_OK))
(define (file-is-executable? f) (access? f X_OK))
(define (file-modification-time f) (stat:mtime (stat f)))
(define (directory? f) (and (file-exists? f)
                            (eq? 'directory (stat:type (stat f)))))

(define (delete-file! f) (if (file? f) (delete-file f)))
(define rename-file! rename-file)
(define copy-file! copy-file)

(define (make-directory! dir)
  (if (not (file-exists? dir)) (mkdir dir)))

(define (delete-directory! f) (if (directory? f) (rmdir f)))

(define current-directory
  (case-lambda
    (() (getcwd))
    ((dir) (chdir dir))))

(define-macro (with-current-directory dir . body)
  (let ((wd (gensym)))
    `(let ((,wd (current-directory)))
       (dynamic-wind
           (lambda () (current-directory ,dir))
           (lambda () ,@body)
           (lambda () (current-directory ,wd))))))

(define (list-dirent dir)
  (let ((ds (opendir dir)))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let loop ((entry (readdir ds)) (res '()))
            (cond ((eof-object? entry) res)
                  ((dot-or-dotdot? entry) (loop (readdir ds) res))
                  (else (loop (readdir ds) (cons entry res))))))
        (lambda () (closedir ds)))))

;;; guile-file.scm ends here
