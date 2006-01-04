;;; @subheading Individual file operations

;;@
;;
;; Returns true if a file or directory exists on the file system at a
;; location specified by @1.  If the operating system does not allow
;; the Scheme program to probe at that location, then a
;; @ref{spells.filesys file-unreachable?,file unreachable error} is
;; signalled.
(define (file-exists? pathname)
  (proc-to-be-defined))

;;@
;;
;; Creates a new directory at @1, or signals a @ref{spells.filesys
;; file-unreachable?,file unreachable error} if the operating system
;; does not permit the Scheme program to create a directory there.  It
;; is an error if there already exists an object at the location
;; designated by @1.
(define (create-directory pathname)
  (proc-to-be-defined))

;;@
;;
;; Deletes the object on the file system at the location specified by
;; @1 (be it a regular file, a directory, or anything else).  If there
;; is no such object, nothing is done.  It is an error if @1
;; designates a directory that is not empty or the operating system
;; does not permit its deletion by the Scheme program.
;; @code{delete-file} signals a @ref{spells.filesys
;; file-unreachable?,file unreachable error} if the Scheme
;; program cannot access the file at the location designated by,
;; whether it satisfies the above conditions or not.
(define (delete-file pathname)
  (proc-to-be-defined))


;;@
;;
;; Renames or moves the file specified by @1 to @2. @2 may be a file
;; pathname, in which case the file is given that exact name; it may
;; also be a directory pathname, in which case the file is moved into
;; that directory.  The object on the file system specified by @1 may
;; be any kind of file: regular, directory, &c.  It is an error if the
;; Scheme program is not permitted by the operating system to perform
;; this operation, @1 does not exist, or @2 designates a file that is
;; not in a directory that already exists & in which files are allowed
;; by the operating system to be created by the Scheme program.  If @1
;; is simply not accessible by the Scheme program, whether or it
;; exists or not, @0 signals a 'file unreachable' error with @1; or,
;; if @2 is in a directory that is not reachable by the Scheme
;; program, it signals a 'file unreachable' error with @2.
(define (rename-file source-pathname target-pathname)
  (proc-to-be-defined))

;;@
;;
;; These all test various attributes of the file specified by @1.
;; @code{file-regular?} tests whether the file is a regular file,
;; rather than a directory or other kind of object on the file
;; system; @code{file-directory?}  tests whether the file is a
;; directory; @code{file-readable?} tests whether the file can be
;; read by the Scheme program, i.e. opening an input port over it
;; (with any of @code{call-with-input-file}, @code{open-input-file},
;; @code{with-input-from-file}, or any other such operation) will not
;; fail; and @code{file-writable?} tests whether a file specified by
;; @1 could be written to, i.e. opening an output port to it
;; (with @code{open-input-file} &c.) would not fail.
;;
;; @code{file-regular?} & @code{file-directory?} return false if
;; there is no object on the file system designated by @1 or signal
;; 'file unreachable' errors if @1 names a file that is inaccessible
;; by the Scheme program.  @code{file-readable?} returns false if the
;; file does not already exist, @code{file-writable?}'s result is
;; unspecified if the file @emph{does} already exist, and they will both
;; return false if the file simply cannot be accessed by the Scheme
;; program, whether the file really exists or not.
(define (file-regular? pathname)
  (proc-to-be-defined))
(define (file-directory? pathname)
  (proc-to-be-defined))
(define (file-readable? pathname)
  (proc-to-be-defined))
(define (file-writable? pathname)
  (proc-to-be-defined))

;;@
;;
;; Returns a SRFI 19 time object that represents the last time that the
;; file on the file system at the location specified by @1 was
;; modified or the time that it was created.  It is an error if the
;; operating system does not allow the Scheme program to access this
;; attribute of the file or the file does not already exist.  If the
;; Scheme program simply cannot access the file, whether it exists or
;; not, @0 signals a 'file unreachable' error.
(define (file-modification-time pathname)
  (proc-to-be-defined))

;;@
;;
;; Returns the number of bytes that the regular file designated by @1
;; contains.  The effect is unspecified if @1 is not a regular file.
;; It is an error if the file does not already exist or the Scheme
;; program is not permitted by the operating system to probe this
;; attribute of the file.  If the Scheme program cannot access the
;; file, whether it exists or not, this signals a 'file unreachable'
;; error.
(define (file-size-in-bytes pathname)
  (proc-to-be-defined))


;;;@subheading Directory content operations

;;@
;;
;; Folds every file in the directory specified by @1 by @2.  That is,
;; for every file in the directory, @2 is passed the full pathname &
;; the current set of state seeds.  @2 should return N+1 values, where
;; N is the number of seeds: a boolean that, if true, specifies that
;; the iteration should continue, or, if false, specifies that the
;; iteration should halt; and the next set of state seeds.  When the
;; iteration is halted, either because @2 returned @code{#f} as its
;; first value or because there are no more files in the directory,
;; the current set of state seeds is returned.  There is no reliable
;; ordering of the filenames passed to @2.
;;
;; If the directory specified by @1 is simply not accessible by the
;; Scheme program, @0 signals a 'file unreachable' error.  It is an
;; error if there is no object specified by @1, the object on
;; the file system is not a directory, or the operating system does
;; not permit the Scheme program to read the directory's contents.
;;
;; @lisp
;;   ;; Return a list of the full pathnames of all files in a directory.
;;   (directory-fold* directory
;;     (lambda (pathname list)
;;       (values #t (cons pathname list)))
;;     '())
;;
;;   ;; Compute a list of the full pathnames of all subdirectories of a
;;   ;; directory.
;;   (directory-fold* directory
;;     (lambda (pathname list)
;;       (values #t (if (file-directory? pathname)
;;                      (cons pathname list)
;;                      list)))
;;     '())
;;
;;   ;; Find the (shallow) sum of the number of bytes in all files in a
;;   ;; directory.
;;   (directory-fold* directory
;;     (lambda (pathname sum)
;;       (values #t (if (file-regular? pathname)
;;                      (+ sum (file-size-in-bytes pathname))
;;                      sum)))
;;     0)
;;
;;   ;; Return the full pathname of the first file in a directory that
;;   ;; satisfies some predicate, or #F if no such file exists.
;;   (directory-fold* directory
;;     (lambda (pathname false)
;;       (if (predicate? pathname)
;;           (values #f pathname)
;;           (values #t #f)))
;;     #f)
;; @end lisp
(define (directory-fold* pathname combiner . seeds)
  (proc-to-be-defined))


;;@
;;
;; Simplified variant of DIRECTORY-FOLD* that does not support premature
;; termination.  This is equivalent to:
;; @lisp
;;  (define (directory-fold pathname combiner . seeds)
;;    (apply
;;      directory-fold* pathname
;;      (lambda (dir-entry . seeds)
;;        (receive new-seeds (apply combiner dir-entry seeds)
;;          (apply values #t new-seeds)))
;;      seeds)
;; @end lisp
;;
;; Some of the above examples are simplified by DIRECTORY-FOLD; e.g., to
;; produce a list of the full pathnames of all files in a directory,
;; @lisp
;;   (directory-fold directory cons '())
;; @end lisp
(define (directory-fold pathname combinder . seeds)
  (proc-to-be-defined))

;;@
;;
;; This is like DIRECTORY-FOLD, but it walks down entire trees of
;; directories.  For each entry in the directory named by @1: if
;; that entry is a non-directory, @2 is passed the full
;; pathname of that file and the current seeds, and the walk of the
;; directory's entries proceeds with the new seeds; if that entry is a
;; directory, @3 is passed the full pathname of that directory
;; & the current seeds, and the seeds it returns are used to recursively
;; descend into the directory.  When the recursive descent returns, the
;; seeds it returned are used to proceed the walk of the enclosing
;; directory's entries.  This could be defined as follows:
;;
;; @lisp
;;   (define (directory-fold-tree pathname file-combiner dir-combiner
;;                                . seeds)
;;     (apply directory-fold pathname
;;            (lambda (pathname . seeds)
;;              (if (file-directory? pathanme)
;;                  (receive new-seeds
;;                           (apply dir-combiner pathname seeds)
;;                    (apply directory-fold-tree pathname
;;                           file-combiner dir-combiner
;;                           new-seeds))
;;                (apply file-combiner pathname seeds)))
;;            seeds))
;; @end lisp
;;
;; However, it is likely to be implemented much more efficiently with
;; respect to the underlying file system.
;;
(define (directory-fold-tree pathname file-combiner dir-combiner . seeds)
  (proc-to-be-defined))
