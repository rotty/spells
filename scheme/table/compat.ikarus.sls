(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (rnrs base)
          (rnrs control)
          (spells hash-tables)
          (spells include))
  
  ;; Ikarus doesn't yet have hashtables, so we use SRFI 69 instead
  (include-file (spells table-srfi-69-impl)))
