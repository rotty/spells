#!r6rs
(library (spells opt-args)
  (export define/named-args
          define/optional-args
          let-optionals
          let-optionals*
          *optional
          opt-lambda)
  (import (rnrs base)
          (spells include))

  (include (spells opt-args)
           (spells opt-args-vanilla)))
