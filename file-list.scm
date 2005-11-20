;; file-list.scm -- Utilities to maintain file lists
;; arch-tag: 5eb4355f-69e8-4c8a-9144-e1d6c7c8f3b7

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Jun 25, 2005 13:40


;;; Comentary:

;;@ File lists hold a list of files, or procedures generating such
;;  lists, and allow global operations on them. A file-list acts as a
;;  procedure producing the list of files it contains when called.

;;; Code:

;;; File list:

;;@ Construct a file list from the given args. A file list holds a
;;list of filenames (and procedures generating filename lists) and
;;allows global operations on them.
(define (make-file-list . args)
  (letrec ((files args)
           (list-files
            (lambda ()
              (apply append
                     (map (lambda (f) (if (procedure? f) (f) (list f)))
                          files))))
           (add-files
            (lambda (ffs)
              (set! files (delete-duplicates (append ffs files)))))
           (del-files
            (lambda ()
              (let ((ffs (sort-list (list-files) string>?)))
                (for-each delete-file! (filter-map
                                        (lambda (f) (and (file? f) f)) ffs))
                (for-each delete-directory!
                          (filter-map (lambda (d) (and (directory? d)
                                                       (null? (list-dirent d))
                                                       d))
                                      (delete-duplicates
                                       (map file-dirname ffs)))))))
           (for-each-file (lambda (proc) (for-each proc (list-files))))
           (map-files (lambda (proc) (map proc (list-files)))))
    (lambda x
      (if (null? x)
          (list-files)
          (case (car x)
            ((add) add-files)
            ((del) del-files)
            ((for-each) for-each-file)
            ((map) map-files)
            (else (error "file-list: unexpected command" x)))))))

;;@ Add a new file to the list if it is not already in there, or a
;; procedure evaluating to a list of files (invoked lazily)
(define add-to-file-list!
  (lambda (fl . files) ((fl 'add) files)))

;;@ Add all files inside directory @2. The optional argument @3
;; specifies a regular expression to be satified by the file names.
(define (add-to-file-list/dir! fl dir . rx)
  (let ((rx (if (null? rx) ".*" (car rx))))
    (add-to-file-list! fl (lambda ()
                            (map (lambda (f)
                                   (make-path dir f))
                                 (find-files dir rx #t))))))

;;@ Delete all files in the list
(define (delete-file-list fl) ((fl 'del)))

;;@ File list traversal procedures.
(define (file-list-for-each fl proc) ((fl 'for-each) proc))
(define (file-list-map fl proc) ((fl 'map) proc))

;;@ Get the least and greatest modification times. These functions
;; return a pair @code{(time . file)}. or @var{#f} if @1 is empty.
(define (file-list-least-modification-time fl)
  (fl-mt fl file-modification-time<))
(define (file-list-greatest-modification-time fl)
  (fl-mt fl file-modification-time>))

(define (fl-mt fl pred)
  (let ((fs (fl)))
    (if (null? fs)
        #f
        (let ((f (car (sort-list fs pred))))
          (cons (file-modification-time f) f)))))


;;; file-list.scm ends here
