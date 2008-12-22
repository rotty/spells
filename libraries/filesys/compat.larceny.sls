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
          (primitives parameterize)
          (prefix (primitives file-exists?
                              current-directory
                              delete-file)
                  la:))

(define x->f x->namestring)

(define (file-exists? pathname)
  (la:file-exists? (x->f pathname)))

(define (create-directory pathname)
  (error 'directory-fold* "please implement me for larceny"))

(define (delete-file pathname)
  ;; FIXME: doesn't work for directories
  (if (file-exists? pathname)
      (la:delete-file (x->f pathname))))

(define (rename-file source-pathname target-pathname)
  (error 'rename-file "please implement me for larceny"))

(define (file-regular? pathname)
  (error 'file-regular? "please implement me for larceny"))

(define (file-symbolic-link? pathname)
  (error 'file-symbolic-link? "please implement me for larceny"))

(define (file-directory? pathname)
  (error 'file-directory? "please implement me for larceny"))

(define (file-readable? pathname)
  (error 'file-readable? "please implement me for larceny"))
(define (file-writable? pathname)
  (error 'file-writable? "please implement me for larceny"))
(define (file-executable? pathname)
  (error 'file-executable? "please implement me for larceny"))

(define (file-modification-time pathname)
  (error 'file-modification-time "please implement me for larceny"))

(define (file-size-in-bytes pathname)
  (error 'file-size-in-bytes "please implement me for larceny"))

(define (directory-fold* pathname combiner . seeds)
  (error 'directory-fold* "please implement me for larceny"))

(define (working-directory)
  (x->pathname (la:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (parameterize ((la:current-directory (x->f (pathname-as-directory (x->pathname dir)))))
       body ...))))

(define (copy-file old-file new-file)
  (error 'copy-file "please implement me for larceny"))

)
