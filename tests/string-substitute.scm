(define-test-suite ssubst-tests
  "string-substitute")

(define-test-case ssubst-tests vectors
  (test-equal "Welcome, Bob."
    (string-substitute "Welcome, {0}." '#("Bob")))
  (test-equal "Today is the Nov 13, 2008"
    (string-substitute "Today is the {1} {2}, {0}" '#("2008" "Nov" "13"))))

(run-test-suite ssubst-tests)
