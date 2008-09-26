;; -*- Mode: Scheme; scheme48-package: spells.filesys; -*-

(define x->f x->namestring)

(define (get-file-info pathname)
  (s48:get-file-info (x->f pathname)))

(define (file-info-last-access file-info)
  (posix-timestamp->time-utc
   (posix:time-seconds (s48:file-info-last-access file-info))))

(define (file-info-last-modification file-info)
  (posix-timestamp->time-utc
   (posix:time-seconds (s48:file-info-last-modification file-info))))
