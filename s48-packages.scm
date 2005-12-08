(define-structure spells.assert (export ((assert) :syntax) cerr cout)
  (open scheme
        spells.error
        spells.port)
  (files assert))

(define-structure spells.format (export format)
  (open formats))

(define-structure spells.opt-args (export ((define/named-args
                                             define/optional-args
                                             let-optionals*
                                             :optional
                                             opt-lambda)
                                             :syntax))
  (open scheme spells.error)
  (files opt-args))

(define-structure spells.table (export make-table
                                       table?
                                       table-ref
                                       table-set!
                                       table-walk
                                       table->alist)
  (open scheme srfi-16 srfi-23 (modify tables (prefix s48:)))
  (files ((pure all) table) ((pure scheme48) table)))

(define-structure spells.hash (export descriptor-hash gc-stamp)
  (open scheme
        (subset primitives (memory-status))
        (subset architecture (memory-status-option))
        enumerated)
  (files ((pure scheme48) hash)))

(define-structure spells.condition spells.condition-interface
  (open scheme srfi-1 srfi-9 srfi-23 conditions srfi-34 srfi-36)
  (files ((pure scheme48) condition)))

(define-structure spells.time-lib spells.srfi-19-interface
  (open scheme
        (subset srfi-1 (reverse!))
        srfi-6
        srfi-8
        srfi-9
        signals
        ascii
        define-record-types
        (modify posix-time (prefix posix:)))
  (files ((pure scheme48) srfi-19)))

(define-structure spells.misc (export identity
                                      compose
                                      eof-object
                                      unspecific
                                      sleep-seconds
                                      thunk?
                                      lookup-environment-variable
                                      sort-list
                                      and-map
                                      or-map
                                      exit)
  (open scheme srfi-23
        (subset primitives (unspecific eof-object))
        i/o
        closures templates bitwise byte-vectors architecture
        posix-process-data
        (subset posix-processes (exit))
        threads
        sort)
  (files ((pure all) misc) ((pure scheme48) misc)))

(define-structure spells.file (export make-path
                                      normalize-path
                                      absolute-path?
                                      dot-or-dotdot?
                                      file-extension
                                      file-basename
                                      file-dirname
                                      file-name-sans-extension
                                      append-extension
                                      replace-extension
                                      file?
                                      directory?
                                      file-is-readable?
                                      file-is-executable?
                                      file-modification-time
                                      file-modification-time>
                                      file-modification-time<
                                      find-exec-path
                                      find-files
                                      copy-file!
                                      rename-file!
                                      delete-file!
                                      current-directory
                                      make-directory!
                                      make-directory*
                                      delete-directory!
                                      fold-dirent
                                      list-dirent
                                      map-dirent
                                      for-each-dirent
                                      filter-dirent
                                      filter-not-dirent
                                      install-file
                                      ((with-current-directory)
                                       :syntax)
                                      call-with-file-and-dir)
  (for-syntax (open scheme destructuring))
  (open scheme srfi-1 srfi-13 srfi-16
        spells.error spells.pregexp spells.misc
        srfi-14 sort posix threads byte-vectors i/o)
  (files ((pure all) file) ((pure scheme48) file)))

(define-structure spells.file-list (export
                                    make-file-list
                                    add-to-file-list!
                                    add-to-file-list/dir!
                                    delete-file-list
                                    file-list-for-each
                                    file-list-map
                                    file-list-least-modification-time
                                    file-list-greatest-modification-time)
  (open scheme srfi-1
        spells.error
        spells.pregexp spells.misc spells.file)
  (files file-list))

(define-structure spells.process (export process?
                                         process-input
                                         process-output
                                         process-errors
                                         spawn-process
                                         run-process
                                         run-process/string
                                         run-process/lines
                                         run-process/sexps
                                         wait-for-process)
  (open scheme srfi-1 srfi-6 srfi-8 srfi-11 srfi-13
        posix-processes
        posix-i/o
        spells.define-record-type
        spells.byte-vectors
        spells.delimited-readers)
  (files ((pure scheme48) process)))

(define-structure spells.ascii (export char->ascii ascii->char
                                       ascii-limit ascii-whitespaces
                                       ascii-upper? ascii-lower?
                                       ascii-uppercase ascii-lowercase)
  (open scheme ascii)
  (files ((pure all) ascii)))

(define-structure spells.pregexp (export pregexp
                                         pregexp-match-positions
                                         pregexp-match
                                         pregexp-split
                                         pregexp-replace
                                         pregexp-replace*)
  (open scheme srfi-1 spells.error)
  (files pregexp))

(define-structure spells.parameter (export make-parameter
                                           ((parameterize) :syntax))
  (open scheme fluids cells signals)
  (files ((pure scheme48) srfi-39)))

(define-structure spells.define-values (export ((define-values)
                                                :syntax))
  (for-syntax (open scheme srfi-1 destructuring))
  (open scheme srfi-8 primitives)
  (files ((pure scheme48) define-values)))

(define-structure spells.match (export ((match match-lambda
                                          match-let match-let*
                                          match-define-values)
                                        :syntax))
  (open scheme spells.define-values)
  (files match))

(define-structure spells.port (export current-error-port
                                      with-output-to-port
                                      with-input-from-port
                                      with-current-ports
                                      force-output
                                      open-output-file/options
                                      ((file-options) :syntax))
  (open scheme i/o
        (subset features (force-output))
        (subset posix-files (file-options file-options-on? file-mode open-file)))
  (files ((pure all) port) ((pure scheme48) port)))

(define-structure spells.alist (export acons assq-ref assv-ref assoc-ref)
  (open scheme)
  (files alist))

(define-structure spells.pretty-print (export pretty-print)
  (open pp))

(define-structure spells.cells (export make-cell
                                       cell?
                                       cell-ref
                                       cell-set!)
  (open cells))

(define-structure spells.error (export error call-error)
  (open signals))

(define-structure spells.weak (export make-weak-pointer
                                      weak-pointer?
                                      weak-pointer-ref

                                      make-population
                                      add-to-population!
                                      population->list
                                      walk-population)
  (open weak))

(define-structure spells.block-io (export read-block write-block write-string)
  (open i/o))

(define-structure spells.bitwise (export bitwise-and bitwise-ior bitwise-xor
                                         bitwise-not
                                         arithmetic-shift
                                         ;;bit-count
                                         )
  (open bitwise))

(define-structure spells.byte-vectors (export make-byte-vector
                                              byte-vector
                                              byte-vector?
                                              byte-vector-length
                                              byte-vector-ref
                                              byte-vector-set!

                                              make-u8vector
                                              u8vector
                                              u8vector?
                                              u8vector-length
                                              u8vector-ref
                                              u8vector-set!
                                              u8vector->list
                                              list->u8vector
                                              u8vector=?
                                              u8vector-compare
                                              u8vector-copy!
                                              u8vector-copy)
  (open scheme byte-vectors)
  (files ((pure scheme48) byte-vectors) (dialects srfi-66-ops)))

(define-structure spells.blobs (export ((endianness) :syntax)
                                       blob?
                                       make-blob
                                       blob-length

                                       blob-u8-ref
                                       blob-s8-ref
                                       blob-u8-set!
                                       blob-s8-set!

                                       blob-uint-ref
                                       blob-sint-ref
                                       blob-uint-set!
                                       blob-sint-set!

                                       blob-u16-ref
                                       blob-s16-ref
                                       blob-u16-native-ref
                                       blob-s16-native-ref
                                       blob-u16-set!
                                       blob-s16-set!
                                       blob-u16-native-set!
                                       blob-s16-native-set!

                                       blob-u32-ref
                                       blob-s32-ref
                                       blob-u32-native-ref
                                       blob-s32-native-ref
                                       blob-u32-set!
                                       blob-s32-set!
                                       blob-u32-native-set!
                                       blob-s32-native-set!

                                       blob-u64-ref
                                       blob-s64-ref
                                       blob-u64-native-ref
                                       blob-s64-native-ref
                                       blob-u64-set!
                                       blob-s64-set!
                                       blob-u64-native-set!
                                       blob-s64-native-set!

                                       blob=?
                                       blob-copy!
                                       blob-copy
                                       blob->u8-list
                                       u8-list->blob
                                       blob->uint-list
                                       blob->sint-list
                                       uint-list->blob
                                       sint-list->blob)
  (open scheme srfi-26 spells.byte-vectors spells.error spells.bitwise)
  (files blobs))

(define-interface spells.field-reader-interface
  (export field-splitter infix-splitter suffix-splitter sloppy-suffix-splitter
	  record-reader
	  field-reader))

(define-interface spells.delimited-readers-interface
  (export read-line
	  read-paragraph
	  read-delimited
	  skip-char-set))

(define-structure spells.field-reader spells.field-reader-interface
  (open scheme srfi-8
	(subset srfi-13 (string-join))
	(subset srfi-14 (char-set?
			 char-set:whitespace
			 char-set
			 x->char-set
			 char-set-complement))
        spells.error
        spells.opt-args
	spells.delimited-readers
        spells.pregexp)
  (files ((pure scheme48) field-reader)))

(define-structure spells.delimited-readers spells.delimited-readers-interface
  (open scheme ports (subset primitives (eof-object))
        srfi-8
        srfi-13
        srfi-14
	spells.byte-vectors
        spells.error
	spells.ascii
        spells.opt-args)
  (files ((pure scheme48) delimited-readers)))

(define-structure spells.define-record-type spells.define-record-type-interface
  (open scheme srfi-9
        (subset define-record-types (define-record-discloser))))

;; arch-tag: b134cf62-6fc4-47ed-8659-c36a7c613e71
