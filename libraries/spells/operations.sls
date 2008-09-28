(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (spells lists)
          (spells procedure-annotations)
          (spells include))
  
  (include ((scheme spells) operations)))
