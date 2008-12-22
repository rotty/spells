#!r6rs

;;@ Stuff that doesn't fit somewhere else.
(library (spells misc)
  (export identity
          compose
          unspecific
          sleep-seconds
          sort-list
          and-map
          or-map
          scheme-dialect)
  (import (rnrs base)
          (rnrs io simple)
          (rnrs sorting)
          (spells include)
          (spells misc compat))

  (include-file ((spells scheme) misc)))
