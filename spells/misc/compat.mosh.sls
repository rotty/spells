#!r6rs
; compat.mosh.sls - (spells misc) compatibility layer for Mosh
; copyright: 2011 David Banks <amoebae@gmail.com>; license: BSD-3-clause

(library (spells misc compat)
  (export sleep-seconds
          scheme-implementation)
  (import (rnrs base)
          (only (mosh concurrent) sleep))

  ; Mosh's sleep from concurrent is millisecond accurate
  (define (sleep-seconds t)
    (sleep (+ (* (exact (truncate t)) #e1e+3)
              (mod (exact (round (* t #e1e+3))) #e1e+3))))
  
  (define (scheme-implementation) 'mosh))
