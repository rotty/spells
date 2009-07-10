#!r6rs
(library (spells testing run-env)
  (export this-directory
          test-environment)
  (import (rnrs base)
          (except (srfi :1 lists) map for-each)
          (srfi :39 parameters)
          (spells pathname)
          (spells include))
  
  (define this-directory (make-parameter #f))

  (define test-environment (make-parameter #f)))
