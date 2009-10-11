;;;; fmt-block.scm -- columnar formatting
;;
;; Copyright (c) 2006-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Columnar formatting
;;
;; A line-oriented formatter.  Takes a list of
;;   (line-fmt1 gen-fmt1 line-fmt2 gen-fmt2 ...)
;; and formats each of the gen-fmt1 formats as columns, printed
;; side-by-side, each line allowing post-processing done by line-fmt1
;; (just use dsp if you want to display the lines verbatim).

;; Continuations come to the rescue to make this work properly,
;; letting us weave the output between different columns without
;; needing to build up intermediate strings.

(define (fmt-columns . ls)
  (lambda (orig-st)
    (call-with-current-continuation
     (lambda (return)
       (define (infinite? x)
         (and (pair? x) (pair? (cdr x)) (pair? (cddr x)) (caddr x)))
       (let ((q1 '())
             (q2 '())
             (remaining (length (remove infinite? ls))))
         (define (enq! proc) (set! q2 (cons proc q2)))
         (define (deq!) (let ((proc (car q1))) (set! q1 (cdr q1)) proc))
         (define (line-init!) (set! q1 (reverse q2)) (set! q2 '()))
         (define (line-done?) (null? q1))
         (define line-buf '())
         (define line-non-empty? #f)
         (define (write-column fmt str finite?)
           (set! line-buf (cons (cons fmt str) line-buf))
           (if finite? (set! line-non-empty? #t)))
         (define (write-line)
           (cond
            (line-non-empty?
             (for-each
              (lambda (x) (set! orig-st (((car x) (cdr x)) orig-st)))
              (reverse line-buf))
             (set! orig-st (nl orig-st))))
           (set! line-buf '())
           (set! line-non-empty? #f)
           (line-init!))
         (define (next cont)
           (enq! cont)
           (cond
            ((line-done?) 
             (write-line)
             (if (not (positive? remaining)) (finish) ((deq!) #f)))
            (else ((deq!) #f))))
         (define (finish)
           (write-line)
           (return orig-st))
         (define (make-empty-col fmt)
           (define (blank *ignored*)
             (write-column fmt "" #f)
             (next blank))    ; infinite loop, next terminates for us
           blank)
         (define (make-col st fmt gen finite?)
           (let ((acc '()))            ; buffer incomplete lines
             (lambda (*ignored*)
               (define (output* str st)
                 (let lp ((i 0))
                   (let ((nli (string-index str #\newline i)))
                     (cond
                      (nli
                       (let ((line
                              (string-concatenate-reverse
                               (cons (substring/shared str i nli) acc))))
                         (set! acc '())
                         (write-column fmt line finite?)
                         (call-with-current-continuation next) 
                         (lp (+ nli 1))))
                      (else
                       (set! acc (cons (substring/shared str i) acc))))))
                 ;; update - don't output or the string port will fill up
                 (fmt-update str st))
               ;; gen threads through it's own state, ignore result
               (gen (fmt-set-writer! (copy-fmt-state st) output*))
               ;; reduce # of remaining finite columns
               (set! remaining (- remaining 1))
               ;; write any remaining accumulated output
               (if (pair? acc)
                   (let ((s (string-concatenate-reverse acc)))
                     (write-column fmt s (and finite? (not (equal? s ""))))))
               ;; (maybe) loop with an empty column in place
               (if (not (positive? remaining))
                   (finish)
                   (next (make-empty-col fmt))))))
         ;; queue up the initial formatters
         (for-each
          (lambda (col)
            (let ((st (fmt-set-port! (copy-fmt-state orig-st)
                                     (open-output-string))))
              (enq! (make-col st (car col) (dsp (cadr col))
                              (not (infinite? col))))))
          ls)
         (line-init!)
         ;; start
         ((deq!) #f))))))

(define (columnar . ls)
  (define (proportional-width? w) (and (number? w) (< 0 w 1)))
  (define (build-column ls)
    (let-optionals* ls ((fixed-width #f)
                        (width #f)
                        (last? #t)
                        (tail '())
                        (gen #f)
                        (prefix '())
                        (align 'left)
                        (infinite? #f))
      (define (scale-width st)
        (max 1 (inexact->exact
                (truncate (* width (- (fmt-width st) fixed-width))))))
      (define (affix x)
        (cond
          ((pair? tail)
           (lambda (str)
             (cat (string-concatenate prefix)
                  (x str)
                  (string-concatenate tail))))
          ((pair? prefix)
           (lambda (str) (cat (string-concatenate prefix) (x str))))
          (else x)))
      (list
       ;; line formatter
       (affix
        (if (and last? (not (pair? tail)) (eq? align 'left))
            dsp
            (if (proportional-width? width)
                (case align
                  ((right)
                   (lambda (str) (lambda (st) ((pad/left (scale-width st) str) st))))
                  ((center)
                   (lambda (str) (lambda (st) ((pad/both (scale-width st) str) st))))
                  (else
                   (lambda (str) (lambda (st) ((pad/right (scale-width st) str) st)))))
                (case align
                  ((right) (lambda (str) (pad/left width str)))
                  ((center) (lambda (str) (pad/both width str)))
                  (else (lambda (str) (pad/right width str)))))))
       ;; generator
       (if (< 0 width 1)
           (lambda (st) ((with-width (scale-width st) gen) st))
           (with-width width gen))
       infinite?
       )))
  (define (adjust-widths ls border-width)
    (let* ((fixed-ls
            (filter (lambda (x) (and (number? (car x)) (>= (car x) 1))) ls))
           (fixed-total (fold + border-width (map car fixed-ls)))
           (scaled-ls (filter (lambda (x) (proportional-width? (car x))) ls))
           (denom (- (length ls) (+ (length fixed-ls) (length scaled-ls))))
           (rest (if (zero? denom)
                     0
                     (/ (- 1 (fold + 0 (map car scaled-ls))) denom))))
      (if (negative? rest)
          (error "fractional widths must sum to less than 1"
                 (map car scaled-ls)))
      (map
       (lambda (col)
         (cons fixed-total
               (if (not (number? (car col))) (cons rest (cdr col)) col)))
       ls)))
  (define (finish ls border-width)
    (apply fmt-columns
           (map build-column (adjust-widths (reverse ls) border-width))))
  (let lp ((ls ls) (strs '()) (align 'left) (infinite? #f)
           (width #t) (border-width 0) (res '()))
    (cond
      ((null? ls)
       (if (pair? strs)
           (finish (cons (cons (caar res)
                               (cons #t (cons (append (reverse strs)
                                                      (caddar res))
                                              (cdddar res))))
                         (cdr res))
                   border-width)
           (finish (cons (cons (caar res) (cons #t (cddar res))) (cdr res))
                   border-width)))
      ((string? (car ls))
       (if (string-index (car ls) #\newline)
           (error "column string literals can't contain newlines")
           (lp (cdr ls) (cons (car ls) strs) align infinite?
               width (+ border-width (string-length (car ls))) res)))
      ((number? (car ls))
       (lp (cdr ls) strs align infinite? (car ls) border-width res))
      ((eq? (car ls) 'infinite)
       (lp (cdr ls) strs align #t width border-width res))
      ((symbol? (car ls))
       (lp (cdr ls) strs (car ls) infinite? width border-width res))
      ((procedure? (car ls))
       (lp (cdr ls) '() 'left #f #t border-width
           (cons (list width #f '() (car ls) (reverse strs) align infinite?)
                 res)))
      (else
       (error "invalid column" (car ls))))))

;; break lines only, don't fmt-join short lines or justify
(define (fold-lines . ls)
  (lambda (st)
    (define output (fmt-writer st))
    (define (kons-in-line str st)
      (let ((len (string-length str))
            (space (- (fmt-width st) (fmt-col st))))
        (cond
          ((or (<= len space) (not (positive? space)))
           (output str st))
          (else
           (kons-in-line
            (substring/shared str space len)
            (output nl-str
                    (output (substring/shared str 0 space) st)))))))
    ((fmt-let
      'writer
      (lambda (str st)
        (let lp ((str str) (st st))
          (let ((nli (string-index str #\newline)))
            (cond
              ((not nli)
               (kons-in-line str st))
              (else
               (lp (substring/shared str (+ nli 1))
                   (output nl-str
                           (kons-in-line
                            (substring/shared str 0 nli)
                            st))))))))
      (apply-cat ls))
     st)))

(define (wrap-fold-words seq knil max-width get-width line . o)
  (let* ((last-line (if (pair? o) (car o) line))
         (vec (if (pair? seq) (list->vector seq) seq))
         (len (vector-length vec))
         (len-1 (- len 1))
         (breaks (make-vector len #f))
         (penalties (make-vector len #f))
         (widths
          (list->vector
           (map get-width (if (pair? seq) seq (vector->list seq))))))
    (define (largest-fit i)
      (let lp ((j (+ i 1)) (width (vector-ref widths i)))
        (let ((width (+ width 1 (vector-ref widths j))))
          (cond
            ((>= width max-width) (- j 1))
            ((>= j len-1) len-1)
            (else (lp (+ j 1) width))))))
    (define (min-penalty! i)
      (cond
        ((>= i len-1) 0)
        ((vector-ref penalties i))
        (else
         (vector-set! penalties i (expt (+ max-width 1) 3))
         (let ((k (largest-fit i)))
           (let lp ((j i) (width 0))
             (if (<= j k)
                 (let* ((width (+ width (vector-ref widths j)))
                        (break-penalty
                         (+ (max 0 (expt (- max-width (+ width (- j i))) 3))
                            (min-penalty! (+ j 1)))))
                   (cond
                     ((< break-penalty (vector-ref penalties i))
                      (vector-set! breaks i j)
                      (vector-set! penalties i break-penalty)))
                   (lp (+ j 1) width)))))
         (if (>= (vector-ref breaks i) len-1)
             (vector-set! penalties i 0))
         (vector-ref penalties i))))
    (define (sub-list i j)
      (let lp ((i i) (res '()))
        (if (> i j)
            (reverse res)
            (lp (+ i 1) (cons (vector-ref vec i) res)))))
    ;; compute optimum breaks
    (vector-set! breaks len-1 len-1)
    (vector-set! penalties len-1 0)
    (min-penalty! 0)
    ;; fold
    (let lp ((i 0) (acc knil))
      (let ((break (vector-ref breaks i)))
        (if (>= break len-1)
            (last-line (sub-list i len-1) acc)
            (lp (+ break 1) (line (sub-list i break) acc)))))))

;; XXXX don't split, traverse the string manually and keep track of
;; sentence endings so we can insert two spaces
(define (wrap-fold str . o)
  (apply wrap-fold-words (string-tokenize str) o))

(define (wrap-lines . ls)
  (define (print-line ls st)
    (nl ((fmt-join dsp ls " ") st)))
  (define buffer '())
  (lambda (st)
    ((fmt-let
      'writer
      (lambda (str st) (set! buffer (cons str buffer)) st)
      (apply-cat ls))
     st)
    (wrap-fold (string-concatenate-reverse buffer)
               st (fmt-width st) string-length print-line)))

(define (justify . ls)
  (lambda (st)
    (let ((width (fmt-width st))
          (output (fmt-writer st))
          (buffer '()))
      (define (justify-line ls st)
        (if (null? ls)
            (nl st)
            (let* ((sum (fold (lambda (s n) (+ n (string-length s))) 0 ls))
                   (len (length ls))
                   (diff (max 0 (- width sum)))
                   (sep (make-string (quotient diff (- len 1)) #\space))
                   (rem (remainder diff (- len 1))))
              (output
               (call-with-output-string
                 (lambda (p)
                   (display (car ls) p)
                   (let lp ((ls (cdr ls)) (i 1))
                     (cond
                       ((pair? ls)
                        (display sep p)
                        (if (<= i rem) (write-char #\space p))
                        (display (car ls) p)
                        (lp (cdr ls) (+ i 1)))))
                   (newline p)))
               st))))
      (define (justify-last ls st)
        (nl ((fmt-join dsp ls " ") st)))
      ((fmt-let
        'writer
        (lambda (str st) (set! buffer (cons str buffer)) st)
        (apply-cat ls))
       st)
      (wrap-fold (string-concatenate-reverse buffer)
                 st width string-length justify-line justify-last))))

(define (fmt-file path)
  (lambda (st)
    (call-with-input-file path
      (lambda (p)
        (let lp ((st st))
          (let ((line (read-line p)))
            (if (eof-object? line)
                st
                (lp (nl ((dsp line) st))))))))))

(define (line-numbers . o)
  (let ((start (if (pair? o) (car o) 1)))
    (fmt-join/range dsp start #f nl-str)))

