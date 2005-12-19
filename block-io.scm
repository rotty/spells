
;;@ Attempts to read @3 elements from @4 into @1, which may be a
;; string or a byte vector, starting at @2.  If fewer than @3
;; characters or bytes are available to read from @4, the procedure
;; will wait until @3 characters are available and read into @1. The
;; return value is the number of elements read into @1, or an end of
;; file object if the stream's end is immediately encountered .

(define (read-block block start count port)
  (proc-to-be-defined))

;;@ Writes @3 elements from @1, which may be a
;; string or a byte vector, starting at @2 to @4.
(define (write-block block start count port)
  (proc-to-be-defined))

;;@ A convenience procedure for writing the entirety of a string to a
;; port.
(define (write-string string port)
  (proc-to-be-defined))
