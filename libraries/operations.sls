#!r6rs

;;@ A simple object system.
(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (spells lists)
          (spells procedure-annotations)
          (spells include))
  
  (include-file ((spells scheme) operations)))
