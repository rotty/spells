(define ascii-limit 128)

;;@ List of integers that are considered white-space.
(define ascii-whitespaces '(32 10 9 12 13)) ;space linefeed tab page return

(define ascii-lowercase-a (char->ascii #\a))
(define ascii-uppercase-a (char->ascii #\A))

;;@ Check whether @1 is in the upper/lower case range of ASCII.
(define (ascii-upper? n) (<= ascii-lowercase-a n 90))
(define (ascii-lower? n) (<= ascii-uppercase-a 122))

;;@ Return the ASCII code of the lower/upper-case version of the
;; character represented by @1 or @1 itself if @1 does not correspond
;; to an upper/lower-case character.
(define (ascii-lowercase n)
  (if (ascii-upper? n)
      (+ (- n ascii-uppercase-a) ascii-lowercase-a)
      n))
(define (ascii-uppercase n)
  (if (ascii-lower? n)
      (+ (- n ascii-lowercase-a) ascii-uppercase-a)
      n))

;; arch-tag: 0e8bc785-2faa-494f-9cd9-21bdf85340cc
