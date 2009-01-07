#!r6rs

;;@ Reading delimited strings.
(library (spells delimited-readers)
  (export read-line
	  read-paragraph
	  read-delimited
	  skip-char-set)
  (import (except (rnrs base) error string-copy string-for-each string->list)
          (rnrs io ports)
          (spells receive)
          (spells strings)
          (spells char-set)
          (spells error)
          (spells opt-args)
          (spells include))

  (include-file ((spells scheme) delimited-readers))
  (include-file ((spells scheme) skip-char-set)))
