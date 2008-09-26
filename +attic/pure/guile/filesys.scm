(read-disable 'case-insensitive)

(define x->f x->namestring)

(define (file-exists? pathname)
  (access? (x->f pathname) F_OK))

(define (create-directory pathname)
  (mkdir (x->f pathname)))

(define guile:delete-file delete-file)
(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (rmdir (x->f pathname))
          (guile:delete-file (x->f pathname)))))

(define guile:rename-file rename-file)
(define (rename-file source-pathname target-pathname)
  (guile:rename-file (x->f source-pathname) (x->f target-pathname)))


(define (file-type-check pathname type)
  (let ((f (x->f pathname)))
    (and (access? f F_OK)
         (eq? type (stat:type (stat f))))))

(define (file-regular? pathname)
  (file-type-check pathname 'regular))

(define (file-symbolic-link? pathname)
  (file-type-check pathname 'symlink))

(define (file-directory? pathname)
  (file-type-check pathname 'directory))

(define (file-readable? pathname)
  (access? (x->f pathname) R_OK))
(define (file-writable? pathname)
  (access? (x->f pathname) W_OK))
(define (file-executable? pathname)
  (access? (x->f pathname) X_OK))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (guile:file-or-directory-modify-seconds (x->f pathname))))

(define (file-size-in-bytes pathname)
  (guile:file-size (x->f pathname)))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (define (full-pathname entry)
    (pathname-with-file pathname entry))
  (let loop ((entries (guile:directory-list (x->f pathname))) (seeds seeds))
    (if (null? entries)
        (apply values seeds)
        (let ((entry (guile:path->string (car entries))))
          (cond ((dot-or-dotdot? entry)
                 (loop (cdr entries) seeds))
                (else
                 (receive (continue? . new-seeds)
                     (apply combiner (full-pathname entry) seeds)
                   (if continue?
                       (loop (cdr entries) new-seeds)
                       (apply values new-seeds)))))))))

(define (working-directory)
  (x->pathname (guile:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (guile:current-directory)))
       (dynamic-wind
           (lambda () (guile:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (guile:current-directory wd)))))))

(define (copy-file old-file new-file)
  (guile:copy-file (x->f old-file) (x->f new-file)))

(read-enable 'case-insensitive)
