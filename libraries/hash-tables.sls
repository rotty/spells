#!r6rs

(library (spells hash-tables)
  (export
   ;; Type constructors and predicate
   make-hash-table hash-table? alist->hash-table 
   ;; Reflective queries
   hash-table-equivalence-function hash-table-hash-function 
   ;; Dealing with single elements
   hash-table-ref hash-table-ref/default hash-table-set!
   hash-table-delete! hash-table-exists?
   hash-table-update! hash-table-update!/default 
   ;; Dealing with the whole contents
   hash-table-size hash-table-keys hash-table-values hash-table-walk
   hash-table-fold hash-table->alist hash-table-copy hash-table-merge! 
   ;;Hashing
   hash string-hash string-ci-hash hash-by-identity)
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
