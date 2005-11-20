(translate "=root" (string-append (this-directory) "/../.."))

(load-system 'testeez)
(load-system 'spells)

(user '(open testeez))
(user '(open testeez.run))

(define (main args)
  (let ((test-specs (call-with-input-file
                        (string-append (this-directory) "/tests.scm")
                      read))
        (tests (cdr args)))
    ;; open structures
    (for-each (lambda (test-spec)
                (if (or (null? tests) (member (car test-spec) tests))
                    (for-each (lambda (structure)
                                (user `(open ,structure)))
                              (cdr test-spec))))
              test-specs)
    ;; run
    (if (null? tests)
        (user `(run (run-tests-and-exit (list ,(this-directory))
                                        ',(the-dialect))))
        (user `(run (run-tests-and-exit
                     ',(map (lambda (test)
                              (string-append (this-directory) "/" test))
                            tests)
                     ',(the-dialect)))))))

;; arch-tag: 5e0e20f4-e0f6-40c6-a91e-12c9ffe22efe
