(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs lists)
          (rnrs unicode)
          (rnrs mutable-pairs)
          (rnrs r5rs)
          (only (spells error) make-error-signaller)
          (spells record-types)
          (spells include))

  (define error (make-error-signaller "SRFI-69"))
  
  ;; Ikarus doesn't yet have hashtables, so we use SRFI 69 instead
  (include-file (spells srfi-69))
  (include-file (spells table-srfi-69-impl)))
