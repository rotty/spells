#!r6rs
(library (spells sharing)
  (export
    write-with-shared-structure
    (rename (write-with-shared-structure write/ss))
    read-with-shared-structure
    (rename (read-with-shared-structure read/ss)))
  (import
    (except (rnrs) read write)
    (only (scheme base) read write parameterize print-graph read-accept-graph))
  
  ;;; NOTE: Not using R6RS read and write probably means the 
  ;;;       full R6RS lexical syntax does not work.  PLT should be
  ;;;       asked to make their print-graph and read-accept-graph
  ;;;       extend their R6RS read and write.
  
  (define write-with-shared-structure
    (case-lambda 
      [(obj)
       (write-with-shared-structure obj (current-output-port))]
      [(obj port)
       (parameterize ([print-graph #t])
         (write obj port))]
      [(obj port optarg)
       (assertion-violation 'write-with-shared-structure
         "this implementation does not support optarg")]))
  
  (define read-with-shared-structure
    (case-lambda
      [()
       (read-with-shared-structure (current-input-port))]
      [(port)
       (parameterize ([read-accept-graph #t]) 
         (read port))]))
  
)
