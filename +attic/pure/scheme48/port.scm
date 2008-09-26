(define (open-output-file/options pathname options)
  (if (file-options-on? options (file-options create))
      (open-file pathname options (file-mode read owner-write))
      (open-file pathname options)))

;; arch-tag: 4f45533b-c56f-4531-b188-195592df99a1
