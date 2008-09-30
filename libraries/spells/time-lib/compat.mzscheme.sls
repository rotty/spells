#!r6rs

(library (spells time-lib compat)
  (export host:time-resolution
          host:current-time 
          host:time-nanosecond 
          host:time-second 
          host:time-gmt-offset)
  (import (rnrs base)
          (prefix (only (scheme)
                        current-seconds current-milliseconds
                        date-time-zone-offset seconds->date) mz:))

  (define host:time-resolution 1000)

  (define (host:current-time)
    (cons (mz:current-seconds) (* (mz:current-milliseconds))))
  (define host:time-nanosecond cdr)
  (define host:time-second car)
  (define host:time-gmt-offset
    (mz:date-time-zone-offset (mz:seconds->date (mz:current-seconds)))))
