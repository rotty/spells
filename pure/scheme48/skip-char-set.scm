(define (skip-char-set skip-chars . maybe-port)
  (let ((port (:optional maybe-port (current-input-port)))
	(cset (x->char-set skip-chars)))
    
    (let lp ((total 0))
      (receive (succ num-read) (buffer-skip-char-set cset port)
	(if (not succ)
	    (+ total num-read)		; eof
	    (begin (peek-char port)	; kludge to fill the buffer
		   (lp  (+ total num-read))))))))

(define (buffer-skip-char-set cset port)
  (let ((the-port-limit (port-limit port)))
    (let lp ((lp-port-index (port-index port)) (i 0))
      (cond ((port-pending-eof? port)
	     (set-port-index! port lp-port-index)
	     (values #f i))
	    ((< lp-port-index the-port-limit)
	     (let ((the-read-char 
		    (ascii->char (byte-vector-ref 
				  (port-buffer port) lp-port-index))))
	       (cond ((char-set-contains? cset the-read-char)
		      (lp (+ lp-port-index 1) (+ i 1)))
		     (else 
		      (set-port-index! port lp-port-index)
		      (values #f i)))))
	    (else (set-port-index! port 0)
		  (set-port-limit! port 0)
		  (values 'port-buffer-exhausted i))))))

