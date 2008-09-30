(library (spells misc compat)
  (export sleep-seconds scheme-dialect exit)
  (import (rnrs base)
          (only (ikarus) nanosleep))

  (define (sleep-seconds t)
    (nanosleep (exact (truncate t)) (mod (exact (round (* t #e1e+9))) #e1e+6)))

  (define (exit status)
    (error 'exit "please implement EXIT for this implementation"))
  
  (define (scheme-dialect)
    'ikarus))
