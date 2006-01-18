;; -*- Mode: Scheme; scheme48-package: spells.filesys; -*-

;; When spells.pathname is ready, this will be defined to X->PATHNAME.
(define x->f values)
(define x->namestring values)

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

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (posix:time-seconds (file-info-last-modification (get-file-info (x->f pathname))))))

(define (file-size-in-bytes pathname)
  (file-info-size (get-file-info (x->f pathname))))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (let ((stream (open-directory-stream (x->f pathname)))
        (namestring (x->namestring pathname)))
    (define (full-pathname entry)
      (make-path namestring entry))
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

