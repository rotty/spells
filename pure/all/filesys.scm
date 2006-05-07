;; -*- Mode: Scheme; scheme48-package: spells.filesys; -*-

(define (directory-fold pathname combiner . seeds)
  (apply
   directory-fold* pathname
   (lambda (dir-entry . seeds)
     (receive new-seeds (apply combiner dir-entry seeds)
       (apply values #t new-seeds)))
   seeds))

;; Naive implementation in terms of DIRECTORY-FOLD
(define (directory-fold-tree pathname file-combiner dir-combiner . seeds)
  (apply directory-fold pathname
         (lambda (pathname . seeds)
           (if (file-directory? pathname)
               (receive new-seeds
                   (apply dir-combiner pathname seeds)
                 (apply directory-fold-tree pathname
                        file-combiner dir-combiner
                        new-seeds))
               (apply file-combiner pathname seeds)))
         seeds))

;;@ Create directories, with intermediary ones when needed.
(define (create-directory* pathname)
  (let ((pathname (x->pathname pathname)))
    (fold (lambda (new path)
            (let ((new-dir (merge-pathnames (make-pathname #f (list new) #f) path)))
              (or (file-exists? new-dir) (create-directory new-dir))
              new-dir))
          (make-pathname #f '() #f)
          (pathname-directory pathname))))

;;@ Vanilla file installation procedure that simply copies the
;; file, creating any needed directory.
(define (install-file src dest)
  (create-directory* dest)
  (copy-file src dest))

(define (call-with-input-file-and-directory pathname proc)
  (let ((pathname (x->pathname pathname)))
    (with-working-directory (directory-namestring pathname)
      (call-with-input-file (file-namestring pathname) proc))))

(define-condition-type &file-unreachable-error &error
  file-unreachable-error?
  (pathname file-unreachable-error-pathname)
  (operator file-unreachable-error-operator))
