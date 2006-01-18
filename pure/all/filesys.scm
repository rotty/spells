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

(define-condition-type &file-unreachable-error &error
  file-unreachable-error?
  (pathname file-unreachable-error-pathname)
  (operator file-unreachable-error-operator))
