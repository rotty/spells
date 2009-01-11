;;; delimited-readers.scm --- Read delimited strings.

;; Code taken from scsh:

;; Copyright (c) 1993-2003 Richard Kelsey and Jonathan Rees
;; Copyright (c) 1994-2003 by Olin Shivers and Brian D. Carlstrom.
;; Copyright (c) 1999-2003 by Martin Gasbichler.
;; Copyright (c) 2001-2003 by Michael Sperber.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


;;; Delimited readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These procedures ran their inner I/O loop in a C primitive in
;;; earlier versions of scsh. In a multi-threaded environment this
;;; causes lots of trouble in case the operation would
;;; block. Therefore the current implementation runs in Scheme but
;;; operates directly on the buffer of the port for speed. This also
;;; allows us to implement the push-back behaviour without a peek/read
;;; pair.
;;;

;;; (read-delimited delims [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a string or the EOF object. DELIM-ACTION determines what to do
;;; with the terminating delimiter:
;;; - PEEK
;;;   Leave it in the input stream for later reading.
;;; - TRIM (the default)
;;;   Drop it on the floor.
;;; - CONCAT
;;;   Append it to the returned string.
;;; - SPLIT
;;;   Return it as a second return value.
;;;


(define (read-delimited delims . args)
  (let-optionals* args ((port         (current-input-port))
                        (delim-action 'trim))
    (%read-delimited delims port delim-action)))

(define (%read-delimited delims port delim-action)
  (let* ((delims (->char-set delims))
           (eof? #f)
           (split #f)
           (result
            (string-unfold
             (lambda (c)
               (or (eof-object? c) (char-set-contains? delims c)))
             values
             (lambda (seed)
               (get-char port)
               (lookahead-char port))
             (lookahead-char port)
             ""
             (lambda (c)
               (if (eof-object? c)
                   (set! eof? #t))
               (case delim-action
                 ((peek)   "")
                 ((concat) (get-char port) (if eof? "" (string c)))
                 ((split)  (get-char port) (set! split c) "")
                 (else     (get-char port) ""))))))
      (if (and eof? (zero? (string-length result)))
          (set! result (eof-object)))
      (if split (values result split) result)))

;;; (read-line [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read in a line of data. Input is terminated by either a newline or EOF.
;;; The newline is trimmed from the string by default.

(define charset:newline (char-set #\newline))

(define (read-line . args)
  (let-optionals* args ((port (current-input-port))
                        (delim-action 'trim))
    (if (eq? delim-action 'trim)
        (get-line port)
        (%read-delimited charset:newline port delim-action))))


;;; (read-paragraph [port handle-delim])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (blank-line? line)
  (string-every char-set:whitespace line))

(define (read-paragraph . args)
  (let-optionals* args ((port         (current-input-port))
                        (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
	(cond ((eof-object? line)
	       (if (eq? handle-delim 'split) (values line line) line))

	      ((blank-line? line) (lp))
              
	      ;; Then, read in non-blank lines.
	      (else
	       (let lp ((lines (list line)))
		 (let ((line (read-line port 'concat)))
		   (if (and (string? line)
			    (not (blank-line? line)))

		       (lp (cons line lines))

		       ;; Return the paragraph
		       (let ((lines->str
                              (lambda (lns) (apply string-append (reverse lns)))))
			 (case handle-delim
			   ((trim) (lines->str lines))

			   ((concat)
			    (lines->str
                             (if (eof-object? line) lines (cons line lines))))

			   ((split)
			    (values (lines->str lines) line))
                           
			   (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))

;; arch-tag: 41794555-4aa5-444d-a16a-86e7187ed7a6
