#!r6rs

(import (rnrs)
        (spells ports)
        (spells gzip))

(define (main argv)
  (call-with-port (standard-output-port)
    (lambda (stdout)
      (for-each (lambda (fname)
                  (call-with-port (open-gz-file-input-port fname)
                    (lambda (in)
                      (copy-port in stdout))))
                (cdr argv)))))

(main (command-line))
