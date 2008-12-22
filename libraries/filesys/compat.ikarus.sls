(library (spells filesys compat)
  (export file-exists?
          create-directory
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
          (rnrs conditions)
          (spells receive)
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

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (define (full-pathname entry)
    (pathname-with-file pathname (pathname-file (x->pathname entry))))
  (let loop ((entries (ik:directory-list (x->f pathname))) (seeds seeds))
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

(define (working-directory)
  (x->pathname (ik:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (ik:current-directory)))
       (dynamic-wind
           (lambda () (ik:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (ik:current-directory wd)))))))

(define (copy-file old-file new-file)
  (error 'copy-file "please implement COPY-FILE for ikarus"))

)
