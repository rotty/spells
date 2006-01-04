;; -*- Mode: Scheme; scheme48-package: spells.pathname; -*-

;;; Record types & disclosers

(define-record-type* pathname
  (make-pathname origin directory file)
  ())

(define-record-discloser pathname
  (lambda (p)
    (list 'pathname (x->namelist p))))

(define-record-type* filename
  (make-filename name extension generation)
  ())

(define-record-discloser filename
  (lambda (f)
    (list 'filename (x->namelist f))))

;;; Generic operations

(define (x->namelist x)
  (cond ((list? x)     x)
        ((pathname? x) (pathname->namelist x))
        ((filename? x) (filename->namelist x))
        (else          (call-error "cannot convert to namelist" x->namelist x))))

(define (x->namestring x)
  (cond ((string? x)   x)
        ((pathname? x) (pathname->namestring x))
        ((filename? x) (filename->namestring x))
        (else          (call-error "cannot convert to namestring" x->namestring x))))

(define (x->filename x)
  (cond ((filename? x) x)
        ((string? x)
         (namestring->filename x))
        (else
         (call-error "cannot convert to filename" x->filename x))))

;;; Filename operations

(define (filename-components f)
  (values (filename-name f) (filename-extension f) (filename-generation f)))

(define (filename->namelist f)
  (receive (name ext gen) (filename-components f)
    (cond (gen  (list name ext gen))
          (ext  (list name ext))
          (name name)
          (else #f))))

;;; Pathname operations

(define (x->pathname x . host-opt)
  (let ((host (and (not (null? host-opt)) (car host-opt))))
    (cond ((pathname? x) x)
          ((string? x)
           (namestring->pathname x host))
          (else
           (call-error "cannot convert to pathname" x->pathname x)))))

(define (pathname-components pathname)
  (values (pathname-host pathname)
          (pathname-base pathname)
          (pathname-directory pathname)
          (pathname-file pathname)))

(define (pathname-with-host pathname host)
  (make-pathname host
                 (pathname-base pathname)
                 (pathname-directory pathname)
                 (pathname-file pathname)))

(define (pathname-with-file pathname file)
  (make-pathname (pathname-host pathname)
                 (pathname-base pathname)
                 (pathname-directory pathname)
                 file))

(define (pathname-with-base pathname base)
  (make-pathname (pathname-host pathname)
                 base
                 (pathname-directory pathname)
                 (pathname-file pathname)))

(define (directory-pathname? pathname)
  (not (pathname-file pathname)))

(define (pathname-as-directory pathname)
  (if (directory-pathname? pathname)
      pathname
      (make-pathname (pathname-host pathname)
                     (pathname-base pathname)
                     (append (pathname-directory pathname)
                             (list (pathname-file pathname)))
                     #f)))

(define (pathname->namelist pathname)
  (receive (host base directory file) (pathname-components pathname)
    (let ((directory (and directory (map filename->namelist directory)))
          (file (and file (filename->namelist file))))
      (cond (host (list host base directory file))
            (base (list base directory file))
            (directory (list directory file))
            ((or (string? file)
                 (symbol? file)) file)
            (else
             (list file))))))

(define merge-pathnames
  (opt-lambda (pathname (default-pathname (working-directory)))
    
    ))
;;; Operations on unportable namestrings.

;; These are currently coded for POSIX-like systems: #\/ is the path
;; separator, the base is '/ for absolute filenames, the generation
;; pathname component is ignored.

(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define namestring-parts
  (let ((char-set:no-slash (char-set-complement (char-set #\/))))
    (lambda (ns)
      (string-tokenize ns char-set:no-slash))))

(define (namestring->directory ns)
  (let loop ((normalized '()) (parts (namestring-parts ns)))
    (if (null? parts)
        (reverse normalized)
        (let ((part (car parts)))
          (cond ((or (string-null? part) (string=? part "."))
                 (loop normalized (cdr parts)))
                ((string=? part "..")
                 (loop (if (or (null? normalized)
                               (string=? (car normalized) ".."))
                           (cons (x->filename part) normalized)
                           (cdr normalized))
                       (cdr parts)))
                (else
                 (loop (cons (x->filename part) normalized) (cdr parts))))))))

(define (namestring->pathname ns host)
  (if (string-null? ns)
      (make-pathname host #f #f #f)
      (let ((base (if (char=? (string-ref ns 0) #\/) '/ #f))
            (last-slash-idx (string-index-right ns #\/)))
        (cond
         ((and last-slash-idx (or (= last-slash-idx (- (string-length ns) 1))
                                  (dot-or-dotdot? (substring/shared ns last-slash-idx))))
          (make-pathname host base (namestring->directory ns) #f))
         (last-slash-idx
          (make-pathname host base
                         (namestring->directory
                          (substring/shared ns 0 last-slash-idx))
                         (namestring->filename
                          (substring/shared ns (+ last-slash-idx 1)))))
         (else
          (make-pathname host base #f (namestring->filename ns)))))))

(define (namestring->filename ns)
  (let ((dot-idx (string-index-right ns #\.)))
    (if dot-idx
        (make-filename
         (substring ns 0 dot-idx)
         (substring ns (+ dot-idx 1) (string-length ns))
         #f)
        (make-filename ns #f #f))))

(define (filename->namestring f)
  (receive (name ext gen) (filename-components f)
    (let ((ext (if ext (string-append "." ext) "")))
      (string-append (or name "") ext))))

(define (pathname->namestring p)
  (receive (host base directory file) (pathname-components p)
    (string-append
     (host+base->string host base)
     (string-join
      (append
       (if directory (map filename->namestring directory) '())
       (if file (list (filename->namestring file)) '("")))
      "/"))))

(define (host+base->string host base)
  (cond ((and host base)
         (string-append (host->string host) ":" (base->string base) "!"))
        (host
         (string-append (host->string host) "/"))
        (base
         (base->string base))
        (else
         "")))

(define (host->string host)
  (cond ((string? host) host)
        ((symbol? host) (symbol->string host))
        ((list? host)
         (receive (scheme host) (apply values host)
           (string-append (symbol->string scheme) "://" host)))))

(define (base->string base)
  (cond ((eq? base '/)    "/")
        ((eqv? base #f)   "")
        ((pathname? base) (pathname->namestring base))
        (else             (call-error "invalid base pathname component"
                                      base->string base))))
