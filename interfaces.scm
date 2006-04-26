;; -*- Mode: Scheme; scheme48-package: (config); -*-

(define-interface spells.srfi-19-interface
  (export time-duration ;; Constants
          time-monotonic
          time-process
          time-tai
          time-thread
          time-utc
          ;; Current time and clock resolution
          current-date
          current-julian-day
          current-modified-julian-day
          current-time
          time-resolution
          ;; Time object and accessors
          make-time
          time?
          time-type
          time-nanosecond
          time-second
          set-time-type!
          set-time-nanosecond!
          set-time-second!
          copy-time
          ;; Time comparison procedures
          time<=?
          time<?
          time=?
          time>=?
          time>?
          ;; Time arithmetic procedures
          time-difference
          time-difference!
          add-duration
          add-duration!
          subtract-duration
          subtract-duration!
          ;; Date object and accessors
          make-date
          date?
          date-nanosecond
          date-second
          date-minute
          date-hour
          date-day
          date-month
          date-year
          date-zone-offset
          date-year-day
          date-week-day
          date-week-number
          ;; Time/Date/Julian Day/Modified Julian Day converters
          date->julian-day
          date->modified-julian-day
          date->time-monotonic
          date->time-tai
          date->time-utc
          julian-day->date
          julian-day->time-monotonic
          julian-day->time-tai
          julian-day->time-utc
          modified-julian-day->date
          modified-julian-day->time-monotonic
          modified-julian-day->time-tai
          modified-julian-day->time-utc
          time-monotonic->date
          time-monotonic->time-tai
          time-monotonic->time-tai!
          time-monotonic->time-utc
          time-monotonic->time-utc!
          time-tai->date
          time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic
          time-tai->time-monotonic!
          time-tai->time-utc
          time-tai->time-utc!
          time-utc->date
          time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic
          time-utc->time-monotonic!
          time-utc->time-tai
          time-utc->time-tai!
          ;; Date to string/string to date converters.
          date->string
          string->date))

(define-interface spells.time-lib-extras-interface
  (export posix-timestamp->time-utc
          time-utc->posix-timestamp))

(define-interface spells.time-lib-interface
  (compound-interface spells.srfi-19-interface
                      spells.time-lib-extras-interface))

(define-interface spells.srfi-34-interface
  (export with-exception-handler
          raise
          ((guard)
           :syntax)))

(define-interface spells.srfi-35-interface
  (export make-condition-type
          condition-type?
          make-condition
          condition?
          condition-has-type?
          condition-ref
          make-compound-condition
          extract-condition
          ((define-condition-type
             condition)
           :syntax)
          &condition
          &message message-condition?
          condition-message
          &serious serious-condition?
          &error error?))

(define-interface spells.srfi-36-interface
  (export
   &i/o-error i/o-error?
   &i/o-port-error i/o-port-error?
   i/o-error-port
   &i/o-read-error i/o-read-error?
   &i/o-write-error i/o-write-error?))

(define-interface spells.srfi-39-interface
  (export make-parameter
          ((parameterize) :syntax)))

(define-interface spells.condition-extras-interface
  (export &irritants irritants? condition-irritants
          &parser-error parser-error?
          parser-error-port
          &end-of-input end-of-input?
          end-of-input-port))

(define-interface spells.condition-interface
  (compound-interface spells.srfi-34-interface
                      spells.srfi-35-interface
                      spells.srfi-36-interface
                      spells.condition-extras-interface))

(define-interface spells.record-types-interface
  (export ((define-record-type define-record-type* define-functional-fields) :syntax)
          define-record-discloser))

(define-interface spells.opt-args-interface
  (export ((define/named-args
             define/optional-args
             let-optionals*
             :optional
             opt-lambda)
           :syntax)))

(define-interface spells.pathname-interface
  (export make-file
          file?
          file-name
          file-type
          file-types
          file-version

          make-pathname
          pathname?
          x->pathname

          pathname-origin
          pathname-directory
          pathname-file
          pathname-with-origin
          pathname-with-directory
          pathname-with-file
          pathname-default
          merge-pathnames
          directory-pathname?
          pathname-as-directory
          pathname-container
          
          directory-namestring
          file-namestring
          origin-namestring
          x->namestring))

;; Legacy interface
(define-interface spells.namestring-interface
  (export make-path
          split-path
          normalize-path
          absolute-path?
          dot-or-dotdot?
          file-extension
          file-name-sans-extension
          replace-extension
          file-basename
          file-dirname
          append-extension))

;; Legacy interface
(define-interface spells.file-interface
  (export file?
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
          call-with-file-and-dir))

;; Legacy interface
(define-interface spells.file-list-interface
  (export make-file-list
          add-to-file-list!
          add-to-file-list/dir!
          delete-file-list
          file-list-for-each
          file-list-map
          file-list-least-modification-time
          file-list-greatest-modification-time))

(define-interface spells.table-interface
  (export make-table
          table?
          table-ref
          table-set!
          table-walk
          table->alist))

(define-interface spells.filesys-interface
  (export file-exists?
          create-directory
          delete-file
          rename-file
          file-regular?
          file-directory?
          file-readable?
          file-writable?
          file-modification-time
          file-size-in-bytes
          
          directory-fold*
          directory-fold
          directory-fold-tree

          file-unreachable-error?
          file-unreachable-error-pathname
          file-unreachable-error-operator))

(define-interface spells.process-interface
  (export process?
          process-input
          process-output
          process-errors
          spawn-process
          run-process
          run-process/string
          run-process/lines
          run-process/sexps
          wait-for-process))

(define-interface spells.misc-interface
  (export identity
          compose
          eof-object
          unspecific
          sleep-seconds
          thunk?
          sort-list
          and-map
          or-map
          exit))

(define-interface spells.sysutils-interface
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          os-name
          os-node-name
          os-release-name
          os-version-name
          machine-name))

(define-interface spells.ascii-interface
  (export char->ascii ascii->char
          ascii-limit ascii-whitespaces
          ascii-upper? ascii-lower?
          ascii-uppercase ascii-lowercase))

(define-interface spells.pregexp-interface
  (export pregexp
          pregexp-match-positions
          pregexp-match
          pregexp-split
          pregexp-replace
          pregexp-replace*))

(define-interface spells.port-interface
  (export current-error-port
          with-output-to-port
          with-input-from-port
          with-current-ports
          force-output
          open-output-file/options
          ((file-options) :syntax)))

(define-interface spells.cells-interface
  (export make-cell
          cell?
          cell-ref
          cell-set!))

(define-interface spells.alist-interface
  (export acons assq-ref assv-ref assoc-ref))

(define-interface spells.weak-interface
  (export make-weak-pointer
          weak-pointer?
          weak-pointer-ref

          make-population
          add-to-population!
          population->list
          walk-population))

(define-interface spells.bitwise-interface
  (export bitwise-and bitwise-ior bitwise-xor
          bitwise-not
          arithmetic-shift
          ;;bit-count
          ))

(define-interface spells.byte-vectors-interface
  (export make-byte-vector
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
          u8vector-copy))

(define-interface spells.blobs-interface
  (export ((endianness) :syntax)
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
          sint-list->blob))

(define-interface spells.match-interface
  (export ((match match-lambda
             match-let match-let*
             match-define-values)
           :syntax)))

(define-interface spells.operations-interface
  (export ((object
            operation
            define-operation) :syntax)))

(define-interface spells.logging-interface
  (export make-log

          log-entry?
          log-entry-level
          log-entry-level-name
          log-entry-object

          make-log-handler
          
          default-log-formatter
          
          configure-logger))

;; arch-tag: a4a455e7-5c3e-4157-b598-a531e44a9e78
