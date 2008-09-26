;;@ A portability library. It offers a single interface to
;; functionality commonly present, but not standardized in various
;; Scheme implementations.
(define-system spells
  (config-files interfaces s48-packages)
  (spedl-files interfaces packages)
  (structures spells.parameter
              spells.define-values
              spells.opt-args
              spells.condition
              spells.cut
              spells.error
              spells.operations
              spells.table
              spells.alist
              spells.vector-lib
              spells.cells
              spells.weak
              spells.byte-vectors
              spells.blobs
              spells.streams
              spells.format
              spells.pretty-print
              spells.ascii
              spells.pregexp
              spells.port
              spells.block-io
              spells.filesys
              spells.sysutils
              spells.process
              spells.bitwise
              spells.assert
              spells.hash
              spells.misc
              spells.match)
  (internal-structures spells.define-record-type*-expander))

;; arch-tag: 8717975c-90a7-4442-87e2-e2ba9efecca3
