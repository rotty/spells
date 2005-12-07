;; file-utils.in.scm -- POSIX file access
; arch-tag: c47fda5c-b01e-454e-83c5-be076defe54c

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

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

;;; Comentary:

;; Interface for accessing POSIX file systems,
;; implementation-independent procedures.

;;; Code:

;;; Path and filenames handling:

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

;; aux function
(define (make-match-proc rx pos def)
  (let ((rx (pregexp rx)))
    (lambda (f)
      (let ((m (pregexp-match rx f)))
        (or (and m (list-ref m pos)) (if def f ""))))))

;;@ Get the given filename's extension, without the leading dot.
;; For instance
;; @example
;;   (file-extension "foo.bar") => "bar"
;; @end example
(define file-extension
  (make-match-proc ".+(\\.)([^./]*)$" 2 #f))

;;@ Return the given filename without extension.
;; E.g.
;; @example
;;   (file-name-sans-extension "/foo/bar/baz.c") => "/foo/bar/baz"
;; @end example
(define file-name-sans-extension
  (make-match-proc "(.+)(\\.[^./]+)$" 1 #t))

;;@ Convenience procedure to substitute a file name's extension.
(define (replace-extension filename new-extension)
  (append-extension (file-name-sans-extension filename) new-extension))

;;@ Append extension to file if it has not already got it.
(define (append-extension f ext)
  (if (string=? ext (file-extension f)) f (string-append f "." ext)))


;;@ Convenience functions for timestamp comparisons
(define (file-modification-time< f1 f2) (fmt f1 f2 <))
(define (file-modification-time> f1 f2) (fmt f1 f2 >))

(define (fmt f1 f2 pred) (pred (file-modification-time f1)
                               (file-modification-time f2)))

;;@ Test whether the given pathname is absolute.
(define (absolute-path? f) (string-prefix? *psep* f))

;;@ Test wheter a pathname is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

;;; Directories:

;;@ Evaluate @var{body} forms using @var{dir} as the current directory.
(define-syntax with-current-directory
  (syntax-rules ()
    ((_ dir . body)
     (let ((wd (current-directory)))
       (dynamic-wind
           (lambda () (current-directory dir))
           (lambda () . body)
           (lambda () (current-directory wd)))))))

;;@ Create directories, with intermediary ones when needed.
(define (make-directory* dir)
  (let ((subdirs (remove string-null? (string-split dir *psepc*))))
    (fold (lambda (new path)
            (let ((new-dir (string-append path *psep* new)))
              (make-directory! new-dir) new-dir))
          "."
          subdirs)))

;;; Finding files:

;;@ Find executable file name in @file{$PATH}.
(define (find-exec-path f)
  (if (absolute-path? f)
      (and (file-is-executable? f) f)
      (let loop ((path
                  (remove string-null? (string-split (lookup-environment-variable "PATH") #\:))))
        (if (null? path) #f
            (let ((fp (make-path (car path) f)))
              (if (file-is-executable? fp) fp (loop (cdr path))))))))

;;@ Find files in @1 matching @2 (a regexp), optionally
;;recursing through subdirectories (if @3 is #t). See also
;;@code{filter-dirent}.
(define (find-files dir regexp recursive)
  (let ((re (if (string? regexp) (pregexp regexp) regexp)))
    (filter-dirent (lambda (e) (pregexp-match re e)) dir recursive)))

;;; Directory traversal

;;@ Generic directory traversal procedure, doing a fold left with
;; @1 as the generating procedure (which takes as first argument
;; the current entry and as second the accumulated result and must
;; return the new value for the latter), and the initial value @2.
;; The optional argument @4, which is @code{#f} by default,
;; indicates whether the traversal must be recursive.
;;
;; The directory where the tranversal is performed is @3. The
;; pathnames passed to @1 do not include this directory as a prefix.
(define (fold-dirent kons knil dir . recursive?)
  (define (do-fold dir val prefix rec)
    (let loop ((dirs (list-dirent dir)) (res val))
      (if (null? dirs) res
          (let ((entry (if (string-null? prefix)
                           (car dirs)
                           (make-path prefix (car dirs))))
                (full-entry (make-path dir (car dirs))))
            (loop (cdr dirs)
                  (kons entry
                        (if (and rec (directory? full-entry))
                            (do-fold full-entry res entry #t)
                            res)))))))
  (do-fold dir knil "" (and (not (null? recursive?)) (car recursive?))))

;;
(define (make-fold-fun conv init)
  (case-lambda
    ((fun dir) (fold-dirent (conv fun) init dir #f))
    ((fun dir recursive?) (fold-dirent (conv fun) init dir recursive?))))

(define map-dirent
  (make-fold-fun (lambda (fun) (lambda (e rest) (cons (fun e) rest))) '()))

(define for-each-dirent
  (make-fold-fun (lambda (fun) (lambda (e ignore) (fun e))) 'ignore))

(define filter-dirent
  (make-fold-fun
   (lambda (fun) (lambda (e rest) (if (fun e) (cons e rest) rest)))
   '()))

(define (filter-not-dirent fun dir)
  (filter-dirent (lambda (e) (not (fun e))) dir))


;;; Copying files:

;;@ Vanilla file installation procedure that simply copies the
;; file, creating any needed directory.
(define (install-file src dest)
  (make-directory* (file-dirname dest))
  (copy-file! src dest))

;;@ Call the one-argument procedure @2 with an input port that
;; reads from @1. During the dynamic extent of @2's
;; execution, the current directory will be @code{(file-dirname
;; @1)}. This is useful for parsing documents that can include
;; files by relative path name.
(define (call-with-file-and-dir filename proc)
  (with-current-directory (file-dirname filename)
    (call-with-input-file (file-basename filename) proc)))

;;; file-utils.in.scm ends here
