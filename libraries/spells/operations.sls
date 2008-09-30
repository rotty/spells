#!r6rs
(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (spells lists)
          (spells procedure-annotations)
          (spells include))
  
  (include (spells operations)))
