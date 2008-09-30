(library (spells sharing)
  (export
    write-with-shared-structure
    (rename (write-with-shared-structure write/ss))
    read-with-shared-structure
    (rename (read-with-shared-structure read/ss)))
  (import
    (rnrs)
    (only (ikarus) print-graph parameterize))
  
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
  
  (define read-with-shared-structure read)
  
)
