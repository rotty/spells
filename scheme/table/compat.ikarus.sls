(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs unicode)
          (rnrs mutable-pairs)
          (rnrs r5rs)
          (spells record-types)
          (spells include))

  ;; Ikarus doesn't yet have hashtables, so we use SRFI 69 instead
  (include-file (spells srfi-69))
  (include-file (spells table-srfi-69-impl)))
