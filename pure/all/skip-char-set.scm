(define (skip-char-set skip-chars . maybe-port)
  (let* ((port (:optional maybe-port (current-input-port)))
         (cset (x->char-set skip-chars)))

      (cond ((not (input-port? port))
             (error "Illegal value -- not an input port." port))
          
             ;; Mighty slow -- we read each char twice (peek first, then read).
             (else (let lp ((i 0))
                     (let ((c (peek-char port)))
                       (cond ((and (char? c) (char-set-contains? cset c))
                              (read-char port)
                              (lp (+ i 1)))
                             (else i))))))))