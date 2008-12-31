#!r6rs

(library (spells match)
  (export match
          match-lambda
          match-let
          match-let*
          match-define-values)
  (import (rnrs base)
          (spells define-values)
          (spells include))

  (include-file ((spells scheme) match)))

