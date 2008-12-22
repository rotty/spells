(define (skip-char-set skip-chars . maybe-port)
  (let* ((port (*optional maybe-port (current-input-port)))
         (cset (->char-set skip-chars)))

    (if (not (input-port? port))
      (error "Illegal value -- not an input port." port))
    
    ;; Mighty slow -- we read each char twice (peek first, then read).
    (let lp ((i 0))
      (let ((c (lookahead-char port)))
        (cond ((and (char? c) (char-set-contains? cset c))
               (get-char port)
               (lp (+ i 1)))
              (else i))))))
