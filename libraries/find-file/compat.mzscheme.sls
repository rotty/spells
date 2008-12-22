#!r6rs

(library (spells find-file compat)
  (export library-search-paths)
  (import (rnrs base)
          (only (mzscheme)
                current-library-collection-paths)
          (scheme mpair))

  (define (library-search-paths)
    (list->mlist (current-library-collection-paths))))
