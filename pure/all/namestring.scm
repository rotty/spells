;; -*- mode: scheme; scheme48-package: spells.namestring; -*-
;;
;; Copyright (C) 2004, 2005-2006 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Oct 25, 2004 10:25

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

;;; Commentary:

;; Operations on POSIX namestrings

;;; Code:

;; Utilities
(define (string-split s c)
  (string-tokenize s (char-set-complement (char-set c))))

(define (drop-trailing-slashes path)
  (string-trim-right path (char-set #\/)))


;;@ Test whether the given pathname is absolute.
(define (absolute-path? f) (string-prefix? *psep* f))

;;@ Test wheter a pathname is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (file-dirname path)
  (if (string=? path "/")
      path
      (let* ((path (drop-trailing-slashes path))
             (last-slash (string-index-right path #\/)))
        (if last-slash
            (let ((dirname (drop-trailing-slashes
                            (string-take path last-slash))))
              (if (string=? dirname "")
                  "/"
                  dirname))
            "."))))

(define (simple-basename f)
  (cond
   ((dot-or-dotdot? f) f)
   ((string-suffix? "/.." f) "..")
   ((string-suffix? "/." f) ".")
   (else
    (let* ((f (drop-trailing-slashes f))
           (last-slash (string-index-right f #\/)))
      (substring f (if last-slash (+ last-slash 1) 0) (string-length f))))))

(define (file-basename path . suffix-opt)
  (let ((path (drop-trailing-slashes path)))
    (let ((last-slash (string-index-right path #\/))
          (suffix (and (not (null? suffix-opt)) (car suffix-opt))))
      (substring path
                 (if last-slash (+ last-slash 1) 0)
                 (if suffix
                     (let ((suffix-start (- (string-length path)
                                            (string-length suffix))))
                       (if (string=? (substring path suffix-start
                                                (string-length path))
                                     suffix)
                           suffix-start
                           (string-length path)))
                     (string-length path))))))


;; Separators used in pathnames
(define *psep* "/")
(define *psepc* #\/)

;;@ Create a normalized path name, simplifying double slashes and
;; @file{.} and @file{..} occurrences.
(define (normalize-path f)
  (define (norm cur rest)
    (if (null? rest) (reverse cur)
        (let ((n (car rest))
              (l (cdr rest)))
          (cond
           ((or (string-null? n)
                (string=? n ".")) (norm cur l))
           ((string=? n "..")
            (norm (cond
                   ((or (null? cur)
                        (string=? (car cur) "..")) (cons n cur))
                   (else (cdr cur)))
                  l))
           ((not (string-skip n #\.))
            (error "Malformed pathname" f))
           (else (norm (cons n cur) l))))))
  (let* ((nodes (string-split f *psepc*))
         (nnodes (norm '() nodes))
         (abs? (absolute-path? f))
         (nn (if (and (null? nnodes) (not abs?)) (list ".") nnodes))
         (fn (string-join nn *psep*)))
    (if abs? (string-append *psep* fn) fn)))

;;@ Create a path name from a list of components.
(define make-path
  (lambda x (normalize-path (string-join x *psep*))))

(define (split-path x)
  (remove string-null? (string-split x *psepc*)))

;; aux function
(define (make-match-proc rx pos nm-k)
  (let ((rx (pregexp rx)))
    (lambda (f)
      (let ((m (pregexp-match rx f)))
        (or (and m (list-ref m pos)) (nm-k f))))))

;;@ Get the given filename's extension, without the leading dot.
;; For instance
;; @example
;;   (file-extension "foo.bar") => "bar"
;; @end example
(define file-extension
  (make-match-proc ".+(\\.)([^./]*)$" 2 (lambda (f) "")))

;;@ Return the given filename without extension.
;; E.g.
;; @example
;;   (file-name-sans-extension "/foo/bar/baz.c") => "/foo/bar/baz"
;; @end example
(define file-name-sans-extension
  (make-match-proc "(.+)(\\.[^./]+)$" 1 (lambda (f) f)))

;;@ Convenience procedure to substitute a file name's extension.
(define (replace-extension filename new-extension)
  (append-extension (file-name-sans-extension filename) new-extension))

;;@ Append extension to file if it has not already got it.
(define (append-extension f ext)
  (if (string=? ext (file-extension f)) f (string-append f "." ext)))
