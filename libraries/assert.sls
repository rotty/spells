#!r6rs

;;@ Assert the truth of an expression.
(library (spells assert)
  (export assert cerr cout)
  (import (except (rnrs base) assert)
          (rnrs io ports)
          (rnrs io simple)
          (spells include))

  (include-file ((spells scheme) assert)))
