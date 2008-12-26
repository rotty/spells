#!r6rs

(library (spells hash-tables)
  (export
   hash-table? make-hash-table hash-table-ref hash-table-ref/default
   hash-table-set! hash-table-update! hash-table-copy
   hash-table-fold hash-table-walk hash-table-keys hash-table-values
   hash-table->alist alist->hash-table hash-table-merge!
   hash-table-delete!)
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
  
  (include-file ((spells scheme) srfi-69)))
