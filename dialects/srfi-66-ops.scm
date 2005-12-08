;; Additional operations from SRFI-66, not defined SRFI-4

;; Code from SRFI-66 reference implementation (c) Michael Sperber (2005)
(define (u8vector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
      (do ((i 0 (+ i 1)))
	  ((= i count))
        (u8vector-set! target
                          (+ target-start i) 
                          (u8vector-ref source (+ source-start i))))
      (do ((i (- count 1) (- i 1)))
	  ((= i -1))
        (u8vector-set! target
                          (+ target-start i) 
                          (u8vector-ref source (+ source-start i))))))

(define (u8vector-copy u8vector)
  (let* ((size (u8vector-length u8vector))
	 (copy (make-u8vector size 0)))
    (u8vector-copy! u8vector 0 copy 0 size)
    copy))

(define (u8vector=? u8vector-1 u8vector-2)
  (let ((size (u8vector-length u8vector-1)))
    (and (= size (u8vector-length u8vector-2))
	 (let loop ((i 0))
	   (or (>= i size)
	       (and (= (u8vector-ref u8vector-1 i)
		       (u8vector-ref u8vector-2 i))
		    (loop (+ 1 i))))))))

(define (u8vector-compare u8vector-1 u8vector-2)
  (let ((length-1 (u8vector-length u8vector-1))
        (length-2 (u8vector-length u8vector-2)))
    (cond
      ((< length-1 length-2) -1)
      ((> length-1 length-2)  1)
      (else
       (let loop ((i 0))
         (if (= i length-1)
             0
             (let ((elt-1 (u8vector-ref u8vector-1 i))
                   (elt-2 (u8vector-ref u8vector-2 i)))
               (cond ((< elt-1 elt-2) -1)
                     ((> elt-1 elt-2)  1)
                     (else (loop (+ i 1)))))))))))

;; arch-tag: 642d99ab-447f-47d7-b7d9-a854e6a91ea1
