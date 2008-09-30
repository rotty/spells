#!r6rs

;;@ Common-Lisp-style @code{format}.
(library (spells format)
  (export format)
  (import (except (rnrs base) error)
          (rnrs unicode)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs r5rs)
          (spells error)
          (spells pretty-print)
          (spells sharing)
          (spells include))

  (define char->ascii char->integer)
  (define ascii->char integer->char)
  
  (include (spells srfi-48)))
