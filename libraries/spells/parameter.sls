#!r6rs

;;@ @uref{http://srfi.schemers.org/srfi-39/srfi-39.html, SRFI 39} - Parameter objects.
(library (spells parameter)
  (export make-parameter
          parameterize)
  (import (rnrs base)
          (rnrs lists)
          (rnrs mutable-pairs)
          (spells include))

  (include (spells parameter)))
