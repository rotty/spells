(#%require (rename mzscheme mz:delete-directory delete-directory)
           (rename mzscheme mz:file-exists? file-exists?)
           (rename mzscheme mz:directory-exists? directory-exists?)
           (rename mzscheme mz:link-exists? link-exists?)
           (rename mzscheme mz:make-directory make-directory)
           (rename mzscheme mz:delete-file delete-file)
           (rename mzscheme mz:rename-file-or-directory rename-file-or-directory)
           (rename mzscheme mz:file-or-directory-permissions file-or-directory-permissions)
           (rename mzscheme mz:file-or-directory-modify-seconds file-or-directory-modify-seconds)
           (rename mzscheme mz:file-size file-size)
           (rename mzscheme mz:directory-list directory-list)
           (rename mzscheme mz:path->string path->string)
           (rename mzscheme mz:copy-file copy-file)
           (rename mzscheme mz:current-directory current-directory))

(define x->f x->namestring)

(define (file-exists? pathname)
  (let ((f (x->f pathname)))
    (or (mz:file-exists? f) (mz:directory-exists? f)
        (mz:link-exists? f))))

(define (create-directory pathname)
  (mz:make-directory (x->f pathname)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (mz:delete-directory (x->f pathname))
          (mz:delete-file (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (mz:rename-file-or-directory (x->f source-pathname) (x->f target-pathname)))

(define (file-regular? pathname)
  (mz:file-exists? (x->f pathname)))

(define (file-symbolic-link? pathname)
  (mz:link-exists? (x->f pathname)))

(define (file-directory? pathname)
  (mz:directory-exists? (x->f pathname)))

(define (file-test-p f p)
  (and (file-exists? f)
       (if (memq p (mz:file-or-directory-permissions f)) #t #f)))

(define (file-readable? pathname)
  (file-test-p (x->f pathname) 'read))
(define (file-writable? pathname)
  (file-test-p (x->f pathname) 'write))
(define (file-executable? pathname)
  (file-test-p (x->f pathname) 'execute))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (mz:file-or-directory-modify-seconds (x->f pathname))))

(define (file-size-in-bytes pathname)
  (mz:file-size (x->f pathname)))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (define (full-pathname entry)
    (pathname-with-file pathname entry))
  (let loop ((entries (mz:directory-list (x->f pathname))) (seeds seeds))
    (if (null? entries)
        (apply values seeds)
        (let ((entry (mz:path->string (car entries))))
          (cond ((dot-or-dotdot? entry)
                 (loop (cdr entries) seeds))
                (else
                 (receive (continue? . new-seeds)
                     (apply combiner (full-pathname entry) seeds)
                   (if continue?
                       (loop (cdr entries) new-seeds)
                       (apply values new-seeds)))))))))

(define (working-directory)
  (x->pathname (mz:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (mz:current-directory)))
       (dynamic-wind
           (lambda () (mz:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (mz:current-directory wd)))))))

(define (copy-file old-file new-file)
  (mz:copy-file (x->f old-file) (x->f new-file)))
