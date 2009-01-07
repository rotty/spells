#!r6rs

;;@ ASCII encoding utilities.
(library (spells ascii)
  (export char->ascii ascii->char
          ascii-limit ascii-whitespaces
          ascii-upper? ascii-lower?
          ascii-uppercase ascii-lowercase)
  (import (rnrs base)
          (spells include))

  (include-file ((spells scheme) ascii)))
