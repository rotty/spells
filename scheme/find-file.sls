(library (spells find-file)
  (export find-file
          library-search-paths)
  (import (rnrs base)
          (spells filesys)
          (spells pathname)
          (spells find-file compat))

  (define (find-file filename paths)
    (let loop ((paths paths))
      (if (null? paths)
          #f
          (let ((full-name (pathname-join (pathname-as-directory (car paths)) filename)))
            (if (file-exists? full-name) full-name (loop (cdr paths))))))))
