(define (call-with-string-input-port s proc)
  (proc (open-string-input-port s)))

(include-file ((spells private fmt) test-fmt))
