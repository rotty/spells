#!r6rs
(library (spells testing run-env)
  (export this-directory
          test-environment)
  (import (rnrs base)
          (srfi :1 lists)
          (spells parameter)
          (spells pathname)
          (spells include))
  
  (define this-directory (make-parameter #f))

  (define test-environment (make-parameter #f)))
