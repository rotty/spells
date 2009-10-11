#!r6rs
(library (spells gc)
  (export make-reaper
          make-weak-cell weak-cell-ref weak-cell?
          collect)
  (import (rnrs base)
          (rnrs control)
          (spells misc)
          (spells gc compat))


)
