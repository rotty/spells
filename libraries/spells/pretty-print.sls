#!r6rs

;;@ A simple pretty-printer for S-expressions.
(library (spells pretty-print)
  (export pretty-print)
  (import (rnrs base)
          (spells sharing))

  ;;@defun pretty-print object [ port ]
  ;; Ouput a pretty-printed representation of @var{object} to @var{port},
  ;; which defaults to @code{(current-output-port)}.
  ;;@end defun
  (define (pretty-print object . port-opt)
    (apply write/ss object port-opt)))

