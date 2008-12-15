(library (spells weak)
  (export make-weak-pointer weak-pointer-ref weak-pointer?)
  (import (rnrs base)
          (only (ikarus) weak-cons weak-pair? bwp-object?))

  (define (make-weak-pointer obj)
    (weak-cons obj #f))

  (define (weak-pointer? thing)
    (weak-pair? thing))

  (define (weak-pointer-ref weak-pointer)
    (let ((obj (car weak-pointer)))
      (and (not (bwp-object? obj)) obj))))
