#!r6rs
(library (spells defrectype-expander)
  (export expand-define-record-type*
          expand-define-functional-fields)
  (import (rnrs base)
          (spells receive)
          (spells lists)
          (spells parameter)
          (spells include))

  (define syntax-error error)
  
  (include-file ((spells scheme) defrectype-expander)))
