;; -*- Mode: Scheme; scheme48-package: spells.filesys; -*-

;; When spells.pathname is ready, this will be defined to X->PATHNAME.
(define x->f x->namestring)

(define (file-exists? pathname)
  (accessible? (x->f pathname) (access-mode exists)))

(define (create-directory pathname)
  (make-directory (x->f pathname) (file-mode owner)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (remove-directory (x->f pathname))
          (unlink (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (rename (x->f source-pathname) (x->f target-pathname)))

(define (file-type-check pathname type)
  (let ((f (x->f pathname)))
    (and (accessible? f (access-mode exists))
         (eq? type (file-type-name (file-info-type (get-file-info f)))))))

(define (file-regular? pathname)
  (file-type-check pathname 'regular))

(define (file-symbolic-link? pathname)
  (file-type-check pathname 'symbolic-link))

(define (file-directory? pathname)
  (file-type-check pathname 'directory))

(define (file-readable? pathname)
  (accessible? (x->f pathname) (access-mode read)))
(define (file-writable? pathname)
  (accessible? (x->f pathname) (access-mode write)))
(define (file-executable? pathname)
  (accessible? (x->f pathname) (access-mode execute)))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc (stat:mtime (stat (x->f pathname)))))

(define (file-size-in-bytes pathname)
  (stat:size (stat (x->f pathname))))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (let ((stream (opendir (x->f pathname))))
    (define (full-pathname entry)
      (pathname-with-file pathname entry))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let loop ((entry (readdir stream)) (seeds seeds))
            (cond ((eqv? entry #f) (apply values seeds))
                  ((dot-or-dotdot? entry)
                   (loop (readdir stream)) seeds)
                  (else
                   (receive (continue? . new-seeds)
                       (apply combiner (full-pathname entry) seeds)
                     (if continue?
                         (loop (readdir stream) new-seeds)
                         (apply values new-seeds)))))))
        (lambda () (closedir stream)))))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (working-directory)))
       (dynamic-wind
           (lambda () (chdir (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (chdir wd)))))))

(define guile:copy-file copy-file)
(define (copy-file old-file new-file) 
   (guile:copy-file (x->f old-file) (x->f new-file)))
