;; -*- Mode: Scheme; scheme48-package: spells.file; -*-

;; Implementations

(define file? fs:file-regular?)
(define file-is-readable? fs:file-readable?)
(define (file-is-executable? f) (accessible? f (access-mode execute)))
(define (file-modification-time f)
  (time-utc->posix-timestamp (fs:file-modification-time f)))
(define (directory? f)
  (fs:file-directory? f))
(define (delete-file! f) (fs:delete-file f))
(define rename-file! fs:rename-file)

(define (copy-file! old-file new-file) 
   (let* ((old-info (get-file-info old-file)) 
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

(define (make-directory! f)
  (if (not (fs:file-exists? f))
      (fs:create-directory f)))

(define delete-directory! fs:delete-file)

(define (current-directory . set-opt)
  (if (null? set-opt)
      (working-directory)
      (set-working-directory! (car set-opt))))

(define-syntax with-current-directory
  (lambda (form r compare)
   (destructure (((with-current-directory dir . body) form))
      `(,(r 'let) ((,(r 'wd) (,(r 'current-directory))))
        (,(r 'dynamic-wind)
         (,(r 'lambda) () (,(r 'current-directory) ,dir))
         (,(r 'lambda) () ,@body)
         (,(r 'lambda) () (,(r 'current-directory) ,(r 'wd))))))))

(define (list-dirent dir)
  (let ((stream (open-directory-stream dir)))
    (dynamic-wind
        (lambda () #t)
        (lambda ()
          (let loop ((entry (read-directory-stream stream)) (res '()))
            (cond ((eqv? entry #f) res)
                  ((dot-or-dotdot? entry)
                   (loop (read-directory-stream stream) res))
                  (else (loop (read-directory-stream stream)
                              (cons entry res))))))
        (lambda () (close-directory-stream stream)))))

;; arch-tag: c643aa5f-39c0-40b1-97db-591f6bfa1d2b
