(define (directory-fold pathname combinder . seeds)
  (apply directory-fold* pathname
         (lambda (dir-entry . seeds)
           (receive new-seeds (apply combiner dir-entry seeds)
             (apply values #t new-seeds)))
      seeds))
