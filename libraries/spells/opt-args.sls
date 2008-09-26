(library (spells opt-args)
  (export define/named-args
          define/optional-args
          let-optionals
          let-optionals*
          *optional
          opt-lambda)
  (import (rnrs base)
          (spells include))

  (include ((scheme spells) opt-args)
           ((scheme spells) opt-args-vanilla)))
