#!r6rs
(library (spells testing run-env)
  (export this-directory
          test-environment)
  (import (rnrs)
          (srfi :39 parameters)
          (spells pathname))
  
  (define this-directory
    (make-parameter (->namestring
                     (pathname-with-file (->pathname (car (command-line))) #f))))

  (define test-environment (make-parameter #f)))
