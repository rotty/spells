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
          (prefix (primitives file-exists?
                              current-directory)
                  la:))

(define x->f x->namestring)

(define (file-exists? pathname)
  (la:file-exists? (x->f pathname)))

(define (create-directory pathname)
  (la:make-directory (x->f pathname)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (la:delete-directory (x->f pathname))
          (la:delete-file (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (la:rename-file (x->f source-pathname) (x->f target-pathname)))

(define (file-regular? pathname)
  (la:file-regular? (x->f pathname)))

(define (file-symbolic-link? pathname)
  (la:file-symbolic-link? (x->f pathname)))

(define (file-directory? pathname)
  (la:file-directory? (x->f pathname)))

(define (file-readable? pathname)
  (la:file-readable? (x->f pathname)))
(define (file-writable? pathname)
  (la:file-writable? (x->f pathname)))
(define (file-executable? pathname)
  (la:file-executable? (x->f pathname)))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (la:file-mtime (x->f pathname))))

(define (file-size-in-bytes pathname)
  (la:file-size (x->f pathname)))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (define (full-pathname entry)
    (pathname-with-file pathname entry))
  (let loop ((entries (la:directory-list (x->f pathname))) (seeds seeds))
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
  (x->pathname (la:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (la:current-directory)))
       (dynamic-wind
           (lambda () (la:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (la:current-directory wd)))))))

(define (copy-file old-file new-file)
  (error "please implement COPY-FILE for ikarus"))

)
