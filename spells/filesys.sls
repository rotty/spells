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
          create-temp-file
          create-temp-directory
          temp-pathname-iterate
          
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
          in-directory-stream
          in-directory
          
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
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1 lists) fold)
          (srfi :8 receive)
          (srfi :19 time)
          (only (srfi :13) string-contains string-unfold)
          (only (srfi :27) random-integer)
          (srfi :98 os-environment-variables)
          (only (spells process) get-process-id)
          (spells string-utils)
          (spells pathname)
          (spells ports)
          (spells filesys compat))


;;@ Foof-loop iterator for directory streams.
(define-syntax in-directory-stream
  (syntax-rules ()
    ((_ (elt-var) (stream-expr) cont . env)
     (cont
      (((stream) stream-expr))         ;Outer bindings
      ()                               ;Loop variables
      (((elt-var) (read-directory-stream stream))) ;Entry bindings
      ((not elt-var))                  ;Termination conditions
      ()                               ;Body bindings
      ()                               ;Final bindings
      . env))))

;;@ Foof-loop iterator for directories.
(define-syntax in-directory
  (syntax-rules ()
    ((_ (elt-var) (pathname-expr) cont . env)
     (cont
      (((stream)                       ;Outer bindings
        (open-directory-stream (->namestring pathname-expr))))
      ()                               ;Loop variables
      (((elt-var)                      ;Entry bindings
        (read-directory-stream stream)))
      ((not elt-var))                  ;Termination conditions
      ()                               ;Body bindings
      ((()                            ;Final bindings
        (begin (close-directory-stream stream)
               (values))))
      . env))))

;;@ Fold @2 over the contents of the directory @1.
;;
;; If the directory @1 has the contents @var{f1}, @var{f2},
;; @var{@dots{}}, this procedure first applies @2 to
;; @code{(pathname-with-file (pathname-as-directory @1) @var{f1})} as
;; first, and @3 as further arguments. The values returned by the
;; application are used as new seed values for the next application of
;; @2. It returns the results of the last application of @2.
(define (directory-fold pathname combiner . seeds)
  (apply
   directory-fold* pathname
   (lambda (dir-entry . seeds)
     (receive new-seeds (apply combiner dir-entry seeds)
       (apply values #t new-seeds)))
   seeds))

(define (directory-fold* pathname combiner . seeds)
  (let* ((dirname (pathname-as-directory pathname))
         (stream (open-directory-stream dirname)))
    (define (full-pathname entry)
      (pathname-with-file dirname (pathname-file (->pathname entry))))
    (let loop ((seeds seeds))
      (let ((entry (read-directory-stream stream)))
        (if (not entry)
            (apply values seeds)
            (receive (continue? . new-seeds)
                     (apply combiner (full-pathname entry) seeds)
              (if continue?
                  (loop new-seeds)
                  (apply values new-seeds))))))))

;; Naive implementation in terms of DIRECTORY-FOLD*
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
  (let ((pathname (pathname-as-directory pathname)))
    (fold (lambda (new path)
            (let ((new-dir (merge-pathnames (make-pathname #f (list new) #f)
                                            path)))
              (or (file-exists? new-dir) (create-directory new-dir))
              new-dir))
          (make-pathname (pathname-origin pathname) '() #f)
          (pathname-directory pathname))))

;;@ Search @var{dir-list}, a list of directories for an occurance of a
;; file as specified by @var{pathname}. If @var{pred} is specified, it
;; must be a single-argument procedure, which is used as an additional
;; predicate that must be satisfied.
(define find-file
  (case-lambda
    ((pathname dir-list pred)
     (let ((pathname (->pathname pathname)))
       (cond ((not (pathname-origin pathname))
              (let loop ((lst dir-list))
                (if (null? lst)
                    #f
                    (let ((path (pathname-join
                                 (pathname-as-directory (car lst))
                                 pathname)))
                      (if (and (file-exists? path)
                               (or (not pred) (pred path)))
                          path
                          (loop (cdr lst)))))))
             ((and (file-exists? pathname)
                   (or (not pred) (pred pathname)))
              pathname)
             (else
              #f))))
    ((pathname dir-list)
     (find-file pathname dir-list #f))))

;;@ Vanilla file installation procedure that simply copies the
;; file, creating any needed directory.
(define (install-file src dest)
  (create-directory* dest)
  (copy-file src dest))

(define (copy-file src-pathname dst-pathname)
  (call-with-port (open-file-input-port (->namestring src-pathname))
    (lambda (in-port)
      (call-with-output-file/atomic dst-pathname 'block #f
        (lambda (out-port)
          (copy-port in-port out-port))))))

;;@ Call @2, with the a file input port corresponding to @1, with a
;; working directory as specified by the directory part of @1.
(define (call-with-input-file-and-directory pathname proc)
  (let ((pathname (->pathname pathname)))
    (with-working-directory (directory-namestring pathname)
      (lambda ()
        (call-with-input-file (file-namestring pathname) proc)))))

;;@ Call @2, with a file output port corresponding to a temporary
;; file. When @2 returns normally, the temporary file is renamed to
;; @1, which normally is an atomic operation.
(define call-with-output-file/atomic
  (case-lambda
    ((pathname buffer-mode transcoder proc)
     (receive (tmp-filename tmp-port)
              (create-temp-file pathname buffer-mode transcoder)
       (guard (c (#t
                  (close-port tmp-port)
                  (delete-file tmp-filename)
                  (raise c)))
         (receive results (call-with-port tmp-port proc)
           (rename-file tmp-filename pathname)
           (apply values results)))))
    ((pathname buffer-mode proc)
     (call-with-output-file/atomic pathname buffer-mode #f proc))
    ((pathname proc)
     (call-with-output-file/atomic pathname 'block (native-transcoder) proc))))

(define create-temp-file
  (case-lambda
    ((template buffer-mode transcoder)
     (temp-pathname-iterate
      (lambda (pathname continue)
        (let ((port (open-file-output-port (->namestring pathname)
                                           (file-options)
                                           buffer-mode
                                           transcoder)))
          (values pathname port)))
      template))
    ((template buffer-mode)
     (create-temp-file template buffer-mode #f))
    ((template)
     (create-temp-file template 'block (native-transcoder)))
    (()
     (create-temp-file (default-temp-template) 'block (native-transcoder)))))

(define create-temp-directory
  (case-lambda
    ((template)
     (temp-pathname-iterate
      (lambda (pathname continue)
        (let ((directory (pathname-as-directory pathname)))
          (create-directory directory)
          directory))
      template))
    (()
     (create-temp-directory (default-temp-template)))))

(define temp-pathname-iterate
  (case-lambda
    ((action template)
     (let ((pathname-generator
            (if (procedure? template)
                template
                (make-pathname-generator template))))
       (let loop ((i 1))
         (guard (c ((i/o-file-already-exists-error? c)
                    (loop (+ i 1))))
           (action (pathname-generator i) (lambda () (loop (+ i 1))))))))
    ((action)
     (temp-pathname-iterate action (default-temp-template)))))

(define (make-pathname-generator template)
  (lambda (count)
    (let* ((template (->pathname template))
           (file (pathname-file template))
           (types (if file (file-types file) '()))
           (fname (if file (file-name file) "")))
      (pathname-with-file
       template
       (make-file (string-substitute
                   (if (exists (lambda (variable)
                                 (string-contains fname variable))
                               '("{count}" "{random}"))
                       fname
                       (string-append fname "-{pid}-{random}.tmp"))
                   `((count . ,count)
                     (random . ,(create-random-string 6))
                     (pid . ,(get-process-id))))
                  types)))))

(define default-temp-template
  (let ((cache #f))
    (lambda ()
      (unless cache
        (set! cache (cond ((get-environment-variable "TMPDIR")
                           => ->pathname)
                          (else
                           (->pathname "/var/tmp/{pid}-{random}.tmp")))))
      cache)))

(define (create-random-string len)
  (string-unfold (lambda (seed)
                   (= len (cdr seed)))
                 car
                 (lambda (seed)
                   (cons (random-char) (+ 1 (cdr seed))))
                 (cons (random-char) 0)))

(define random-char
  (let ((alphabet
         "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (lambda ()
      (string-ref alphabet (random-integer (string-length alphabet))))))

(define-condition-type &file-unreachable-error &error
  file-unreachable-error? make-file-unreachable-error
  (pathname file-unreachable-error-pathname)
  (operator file-unreachable-error-operator))

)
