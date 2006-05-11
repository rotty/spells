;; -*- Mode: Scheme; scheme48-package: spells.filesys; -*-

;; When spells.pathname is ready, this will be defined to X->PATHNAME.
(define x->f x->namestring)

(define (file-exists? pathname)
  (accessible? (x->f pathname) (access-mode exists)))

(define (create-directory pathname)
  (make-directory (x->f pathname) (file-mode owner)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-regular? pathname)
          (unlink (x->f pathname))
          (remove-directory (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (rename (x->f source-pathname) (x->f target-pathname)))

(define (file-type-check pathname type)
  (let ((f (x->f pathname)))
    (and (accessible? f (access-mode exists))
         (eq? type (file-type-name (file-info-type (get-file-info f)))))))

(define (file-regular? pathname)
  (file-type-check pathname 'regular))

(define (file-directory? pathname)
  (file-type-check pathname 'directory))

(define (file-readable? pathname)
  (accessible? (x->f pathname) (access-mode read)))
(define (file-writable? pathname)
  (accessible? (x->f pathname) (access-mode write)))
(define (file-executable? pathname)
  (accessible? (x->f pathname) (access-mode execute)))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (posix:time-seconds (file-info-last-modification (get-file-info (x->f pathname))))))

(define (file-size-in-bytes pathname)
  (file-info-size (get-file-info (x->f pathname))))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (let ((stream (open-directory-stream (x->f pathname))))
    (define (full-pathname entry)
      (pathname-with-file pathname entry))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let loop ((entry (read-directory-stream stream)) (seeds seeds))
            (cond ((eqv? entry #f) (apply values seeds))
                  ((dot-or-dotdot? entry)
                   (loop (read-directory-stream stream)) seeds)
                  (else
                   (receive (continue? . new-seeds)
                       (apply combiner (full-pathname entry) seeds)
                     (if continue?
                         (loop (read-directory-stream stream) new-seeds)
                         (apply values new-seeds)))))))
        (lambda () (close-directory-stream stream)))))

(define-syntax with-working-directory
  (lambda (form r compare)
   (destructure (((with-working-directory dir . body) form))
      `(,(r 'let) ((,(r 'wd) (,(r 'working-directory))))
        (,(r 'dynamic-wind)
         (,(r 'lambda) () (,(r 'set-working-directory!)
                           (,(r 'x->namestring) (,(r 'pathname-as-directory)
                                                 (,(r 'x->pathname) ,dir)))))
         (,(r 'lambda) () ,@body)
         (,(r 'lambda) () (,(r 'set-working-directory!) ,(r 'wd))))))))

(define (copy-file old-file new-file) 
   (let* ((old-file (x->f old-file))
          (new-file (x->f new-file))
          (old-info (get-file-info old-file)) 
          (old-port (open-file old-file (file-options read-only))) 
          (new-port (open-file new-file (file-options write-only create truncate) 
                               (file-info-mode old-info))) 
          (buf-size 4000) 
          (buffer (make-byte-vector buf-size 0))) 
     (let loop () 
       (let ((n (read-block buffer 0 buf-size old-port))) 
         (cond ((not (eof-object? n)) 
                (write-block buffer 0 n new-port) 
                (loop))))) 
     (close-input-port old-port) 
     (close-output-port new-port)))
