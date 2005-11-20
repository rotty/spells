;; Utilities
(define (string-split s c)
  (string-tokenize s (char-set-complement (char-set c))))

(define (drop-trailing-slashes path)
  (string-trim-right path (char-set #\/)))

;; Implementations

(define (file-dirname path)
  (if (string=? path "/")
      path
      (let* ((path (drop-trailing-slashes path))
             (last-slash (string-index-right path #\/)))
        (if last-slash
            (let ((dirname (drop-trailing-slashes
                            (string-take path last-slash))))
              (if (string=? dirname "")
                  "/"
                  dirname))
            "."))))

(define (simple-basename f)
  (cond
   ((dot-or-dotdot? f) f)
   ((string-suffix? "/.." f) "..")
   ((string-suffix? "/." f) ".")
   (else
    (let* ((f (drop-trailing-slashes f))
           (last-slash (string-index-right f #\/)))
      (substring f (if last-slash (+ last-slash 1) 0) (string-length f))))))

(define file-basename
  (case-lambda
    ((f) (simple-basename f))
    ((f ext) (let ((bn (simple-basename f)))
               (if (string-suffix? ext bn)
                   (string-drop-right bn (string-length ext))
                   bn)))))

(define (file-basename path . suffix-opt)
  (let ((path (drop-trailing-slashes path)))
    (let ((last-slash (string-index-right path #\/))
          (suffix (and (not (null? suffix-opt)) (car suffix-opt))))
      (substring path
                 (if last-slash (+ last-slash 1) 0)
                 (if suffix
                     (let ((suffix-start (- (string-length path)
                                            (string-length suffix))))
                       (if (string=? (substring path suffix-start
                                                (string-length path))
                                     suffix)
                           suffix-start
                           (string-length path)))
                     (string-length path))))))

(define (file-type-check f pred)
  (and (accessible? f (access-mode exists))
       (pred (file-type-name (file-info-type (get-file-info f))))))

(define (file? f) (file-type-check f (lambda (type)
                                      (not (eq? type 'directory)))))
(define (file-is-readable? f) (accessible? f (access-mode read)))
(define (file-is-executable? f) (accessible? f (access-mode execute)))
(define (file-modification-time f)
  (time-seconds (file-info-last-modification (get-file-info f))))
(define (directory? f)
  (file-type-check f (lambda (type) (eq? type 'directory))))
(define (delete-file! f) (if (file? f) (unlink f)))
(define rename-file! rename)

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

(define (make-directory! dir)
  (if (not (accessible? dir (access-mode exists)))
      (make-directory dir (file-mode owner))))

(define (delete-directory! dir)
  (if (directory? dir)
      (remove-directory dir)))

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
