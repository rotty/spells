;; -*- Mode: Scheme; scheme48-package: (config); -*-

(define-structure spells.assert (export ((assert) :syntax) cerr cout)
  (open scheme
        spells.error
        spells.port)
  (files assert))

(define-structure spells.format (export format)
  (open formats))

(define-structure spells.cut (export ((cut cute) :syntax))
  (open srfi-26))

(define-structure spells.opt-args spells.opt-args-interface
  (open scheme spells.error)
  (files opt-args))

(define-structure spells.table spells.table-interface
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
  (files ((pure all) condition)))

(define-structure spells.time-lib spells.time-lib-interface
  (open scheme
        (subset srfi-1 (reverse!))
        srfi-6
        srfi-8
        srfi-9
        signals
        ascii
        (modify posix-time (prefix posix:)))
  (files ((pure scheme48) srfi-19) ((pure all) time-lib)))

(define-structure spells.misc spells.misc-interface
  (open scheme srfi-23
        (subset primitives (unspecific eof-object))
        i/o
        closures templates bitwise byte-vectors architecture
        posix-process-data
        (subset posix-processes (exit))
        threads
        sort)
  (files ((pure all) misc) ((pure scheme48) misc)))


(define-structure spells.pathname spells.pathname-interface
  (open scheme srfi-1 srfi-8 srfi-13 srfi-14
        spells.error
        spells.opt-args
        spells.record-types
        spells.operations)
  (files pathname))

(define-structure spells.filesys spells.filesys-interface
  (for-syntax (open scheme destructuring))
  (open scheme srfi-1 srfi-8
        posix-files
        (modify posix-time (prefix posix:))
        spells.byte-vectors
        spells.condition
        spells.pathname
        spells.time-lib
        spells.block-io)
  (files ((pure scheme48) filesys) ((pure all) filesys)))

(define-structure spells.posix-filesys spells.posix-filesys-interface
  (open scheme
        (modify posix-files
                (rename (get-file-info s48:get-file-info)
                        (file-info-last-access s48:file-info-last-access)
                        (file-info-last-modification s48:file-info-last-modification)))
        (modify posix-time (prefix posix:))
        posix-users
        spells.pathname
        spells.time-lib)
  (files ((pure scheme48) posix-filesys)))

(define-structure spells.sysutils spells.sysutils-interface
  ;; note: it should be POSIX-PLATFORM-NAMES instead of POSIX, but
  ;; for some reason s48 does not recognise the former structure name
  (open scheme srfi-1 srfi-13 srfi-14
        posix-process-data posix
        spells.pathname spells.filesys)
  (files ((pure all) sysutils) ((pure scheme48) sysutils)))

(define-structure spells.process spells.process-interface
  (open scheme srfi-1 srfi-6 srfi-8 srfi-11 srfi-13
        posix-processes
        posix-i/o
        spells.port
        spells.error
        spells.record-types
        spells.byte-vectors
        spells.delimited-readers
        spells.pathname)
  (files ((pure scheme48) process)))

(define-structure spells.ascii spells.ascii-interface
  (open scheme ascii)
  (files ((pure all) ascii)))

(define-structure spells.pregexp spells.pregexp-interface
  (open scheme srfi-1 spells.error)
  (files pregexp))

(define-structure spells.parameter spells.srfi-39-interface
  (open scheme fluids cells signals)
  (files ((pure scheme48) srfi-39)))

(define-structure spells.define-values (export ((define-values)
                                                :syntax))
  (open scheme lib42.define-values))

(define-structure spells.match (export ((match match-lambda
                                          match-let match-let*
                                          match-define-values)
                                        :syntax))
  (open scheme lib42.match))

(define-structure spells.port spells.port-interface
  (open scheme i/o
        (subset features (force-output))
        (subset posix-files (file-options file-options-on? file-mode open-file)))
  (files ((pure all) port) ((pure scheme48) port)))

(define-structure spells.alist spells.alist-interface
  (open scheme)
  (files alist))

(define-structure spells.pretty-print (export pp)
  (open (modify pp (rename (p pp)))))

(define-structure spells.cells spells.cells-interface
  (open cells))

(define-structure spells.error (export error call-error)
  (open signals))

(define-structure spells.weak spells.weak-interface
  (open weak))

(define-structure spells.block-io (export read-block write-block write-string)
  (open i/o))

(define-structure spells.bitwise spells.bitwise-interface
  (open bitwise))

(define-structure spells.byte-vectors spells.byte-vectors-interface
  (open scheme byte-vectors)
  (files ((pure scheme48) byte-vectors) (dialects srfi-66-ops)))

(define-structure spells.blobs spells.blobs-interface
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

(define-structure spells.define-record-type*-expander (export expand-define-record-type*
                                                              expand-define-functional-fields)
  (open scheme srfi-1 destructuring fluids signals receiving)
  (files ((pure scheme48) defrectype*)))

(define-structure spells.record-types spells.record-types-interface
  (open scheme srfi-8 srfi-9
        (subset define-record-types (define-record-discloser)))
  (for-syntax (open scheme spells.define-record-type*-expander))
  (begin (define-syntax define-record-type*
           expand-define-record-type*
           (BEGIN DEFINE DEFINE-RECORD-TYPE)))
  (begin (define-syntax define-functional-fields
           expand-define-functional-fields
           (BEGIN DEFINE RECEIVE))))

(define-structure spells.operations spells.operations-interface
  (open scheme
        spells.error)
  (files operations))

(define-structure spells.logging spells.logging-interface
  (open scheme srfi-1
        (subset spells.time-lib (current-time))
        spells.error
        spells.opt-args
        spells.record-types)
  (files logging))

(define-structure spells.streams spells.streams-interface
  (open scheme srfi-1
        spells.cells
        spells.record-types
        spells.error)
  (files streams))

(define-structure spells.vector-lib spells.vector-lib-interface
  (open scheme
        spells.error)
  (files vector-lib))

;; arch-tag: b134cf62-6fc4-47ed-8659-c36a7c613e71
