#!r6rs

(library (spells testing)
  (export
   define-test-suite
   make-test-suite
   test-suite?
   test-suite/name
   test-suite/description
   test-suite/tests

   define-test-case
   test-case
   make-test-case
   test-case?
   test-case/name
   test-case/description
   test-case/constructor

   add-test!
   run-test-case
   run-test-suite
   run-test
   find-test

   test-predicate
   test-compare
   test-eq
   test-eqv
   test-equal

   ;; Reexport these for convenience.  Ordinarily reexportation is
   ;; anathema, but I think that here it is probably safe, because
   ;; most users will never use TESTING-PARAMETERS anyway.
   test-failure
   test-failure:predicate-datum
   test-failure:compare-datum

   test-verbosity      with-test-verbosity     set-test-verbosity!
   test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!)

  (import (except (rnrs base) error)
          (rnrs lists)
          (rnrs mutable-pairs)
          (xitomatl srfi receive)
          (spells define-values)
          (spells record-types)
          (spells include)
          (spells syn-param)
          (only (spells error) make-error-signaller)
          (spells testing parameters))

  (define error (make-error-signaller "trc-testing"))

  (include-file ((spells scheme) trc-testing))

  )
