(library (spells time-lib compat)
  (export
    format
    host:time-resolution
    host:current-time 
    host:time-nanosecond 
    host:time-second 
    host:time-gmt-offset)
  (import
    (r5rs) 
    (rnrs)
    (larceny load)
    (primitives r5rs:require current-utc-time timezone-offset)    
    (spells format))
  
  (define-record-type time (fields secs usecs))
  
  ;; Larceny uses gettimeofday() which gives microseconds,
  ;; so our resolution is 1000 nanoseconds
  (define host:time-resolution 1000)
  
  (define (host:current-time)
    (let-values ([(secs usecs) (current-utc-time)])
      (make-time secs usecs)))
  
  (define (host:time-nanosecond t)
    (* (time-usecs t) 1000))
  
  (define (host:time-second t)
    (time-secs t))
  
  (define (host:time-gmt-offset t) 
    (timezone-offset (time-secs t)))
  
  (r5rs:require 'time)

)
