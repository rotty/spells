;;; filesys.scm --- Filesystem interface.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This file contains the non-primitive procedures that can be defined
;; in terms of primitives.

;;; Code:
#!r6rs

;;@ File system interface.
(library (spells filesys)
  (export file-exists?
          create-directory
          create-directory*
          delete-file
          rename-file
          copy-file
          install-file
          create-symbolic-link
          create-hard-link

          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-fold*
          directory-fold
          directory-fold-tree*
          directory-fold-tree

          file-unreachable-error?
          file-unreachable-error-pathname
          file-unreachable-error-operator

          working-directory
          with-working-directory

          call-with-input-file-and-directory
          call-with-output-file/atomic

          find-file
          library-search-paths)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs exceptions)
          (srfi :8 receive)
          (srfi :1 lists)
          (spells pathname)
          (spells time-lib)
          (spells filesys compat))


(define (directory-fold pathname combiner . seeds)
  (apply
   directory-fold* pathname
   (lambda (dir-entry . seeds)
     (receive new-seeds (apply combiner dir-entry seeds)
       (apply values #t new-seeds)))
   seeds))

;; Naive implementation in terms of DIRECTORY-FOLD
(define (directory-fold-tree* pathname file-combiner dir-combiner . seeds)
  (apply directory-fold* pathname
         (lambda (pathname . seeds)
           (if (file-directory? pathname)
               (receive (new-fc new-dc proceed . new-seeds)
                        (apply dir-combiner pathname seeds)
                 (cond ((and new-fc new-dc)
                        (receive newest-seeds
                                 (apply directory-fold-tree*
                                        (pathname-as-directory pathname)
                                        (if (eqv? new-fc #t) file-combiner new-fc)
                                        (if (eqv? new-dc #t) dir-combiner new-dc)
                                        new-seeds)
                          (if proceed
                              (apply proceed newest-seeds)
                              (apply values #t newest-seeds))))
                       ((or new-fc new-dc)
                        (apply values #t new-seeds))
                       (else
                        (apply values #f new-seeds))))
               (apply file-combiner pathname seeds)))
         seeds))

(define (directory-fold-tree pathname file-combiner dir-combiner . seeds)
  (apply directory-fold-tree* pathname
         (lambda (file-entry . seeds)
           (receive new-seeds (apply file-combiner file-entry seeds)
             (apply values #t new-seeds)))
         (lambda (dir-entry . seeds)
           (receive new-seeds (apply dir-combiner dir-entry seeds)
             (apply values #t #t #f new-seeds)))
         seeds))

;;@ Create directories, with intermediary ones when needed.
(define (create-directory* pathname)
  (let ((pathname (pathname-as-directory (x->pathname pathname))))
    (fold (lambda (new path)
            (let ((new-dir (merge-pathnames (make-pathname #f (list new) #f)
                                            path)))
              (or (file-exists? new-dir) (create-directory new-dir))
              new-dir))
          (make-pathname (pathname-origin pathname) '() #f)
          (pathname-directory pathname))))

;;@ Search @1, a list of directories for an occurance of a file as
;; specified by pathname.
(define (search-directory-list dir-list pathname)
  (let ((pathname (x->pathname pathname)))
    (cond ((pathname-origin pathname)
           pathname)
          (else
           (let loop ((lst dir-list))
             (if (null? lst)
                 #f
                 (let ((path (merge-pathnames pathname (car lst))))
                   (if (file-exists? path)
                       path
                       (loop (cdr lst))))))))))

;;@ Vanilla file installation procedure that simply copies the
;; file, creating any needed directory.
(define (install-file src dest)
  (create-directory* dest)
  (copy-file src dest))

;;@ Call @2, with the a file input port corresponding to @1, with a
;; working directory as specified by the directory part of @1.
(define (call-with-input-file-and-directory pathname proc)
  (let ((pathname (x->pathname pathname)))
    (with-working-directory (directory-namestring pathname)
      (call-with-input-file (file-namestring pathname) proc))))

;;@ Call @2, with a file output port corresponding to a temporary
;; file. When @2 returns normally, the temporary file is renamed to
;; @1, which normally is an atomic operation.
(define (call-with-output-file/atomic pathname proc)
  (receive (tmp-filename tmp-port) (create-temp-file pathname)
    (guard (c (#t
               (close-port tmp-port)
               (delete-file tmp-filename)
               (raise c)))
      (receive results (call-with-port tmp-port proc)
        (rename-file tmp-filename pathname)
        (apply values results)))))

(define create-temp-file
  (let ((count 1))
    (lambda (path)
      (let ((prefix (file-namestring path)))
        (let loop ((i count))
          (let ((pathname (pathname-with-file
                           path
                           (string-append prefix (number->string i) ".tmp"))))
            (guard (c ((i/o-file-already-exists-error? c)
                       (loop (+ i 1))))
              (let ((port (open-file-output-port (x->namestring pathname)
                                                 (file-options)
                                                 'block
                                                 (native-transcoder))))
                (set! count (+ i 1))
                (values pathname port)))))))))

(define-condition-type &file-unreachable-error &error
  file-unreachable-error? make-file-unreachable-error
  (pathname file-unreachable-error-pathname)
  (operator file-unreachable-error-operator))

;;@ Find @1 in the list of directories @2.
(define (find-file pathname paths)
  (let loop ((paths paths))
    (if (null? paths)
        #f
        (let ((full-name (pathname-join (pathname-as-directory (car paths))
                                        pathname)))
          (if (file-exists? full-name) full-name (loop (cdr paths)))))))

)
