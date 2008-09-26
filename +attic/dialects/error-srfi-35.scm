(define (error reason . args)
  (raise (condition (&error)
                    (&message (message reason))
                    (&irritants (values args)))))

;; arch-tag: 5d5ef4e0-a72e-477a-b004-5b54dbbb3100
