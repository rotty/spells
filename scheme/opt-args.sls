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

  (include-file (spells opt-args))
  (include-file (spells opt-args-vanilla)))
