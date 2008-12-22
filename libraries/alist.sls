#!r6rs

;;@ Association list utilities.
(library (spells alist)
  (export acons assq-ref assv-ref assoc-ref)
  (import (rnrs base)
          (rnrs lists)
          (spells include))
  
  (include-file ((spells scheme) alist)))
