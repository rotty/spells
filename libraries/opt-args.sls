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

  (include-file ((spells scheme) opt-args))
  (include-file ((spells scheme) opt-args-vanilla)))
