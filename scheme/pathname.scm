;; -*- Mode: Scheme; scheme48-package: spells.pathname; -*-

;;;; Portable Pathname Abstraction
;;;; Version 3 (beta)

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; /Pathnames/ are platform-independent representations of locations
;;; of objects on file systems.  A pathname consists of three
;;; components, each of which is optional:
;;;
;;;   - The /origin/ is the place from which the path begins.  It might
;;;     be the Unix root directory, a user's home directory, a DOS
;;;     device/drive, an Apollo logical name, a Unix environment
;;;     variable, a VMS host, &c.  Origins can also have expansions
;;;     defined in Emacs.  A pathname with a null origin is a relative
;;;     pathname.
;;;
;;;   - The /directory/ is a list of directory names from the origin
;;;     leading up to the file.  Each directory name is as string or a
;;;     symbol.
;;;
;;;   - The /filename/ is the name of the file itself along with the type
;;;     and version.  The name is a string or a symbol.  There may be
;;;     zero or more types, each of which is also a string or a symbol.
;;;     The version is either a non-negative integer or the symbol
;;;     `newest'.

;++ Still missing:
;++
;++   - non-Unix file system types
;++       There should be separate pathname-unix.el, pathname-mac.el,
;++       and pathname-dos.el files at least.
;++   - file merging (see losing definition of MERGE-FILES)
;++   - enough pathnames
;++   - pathname expansion
;++   - truename operation

;;;; Utilities

;;; This section must come first so that ENFORCE is defined before
;;; compiling the functions in the file that use it.

;;@
;; Return t if @1 is a string or a symbol and nil if not."
(define (string-or-symbol? object)
  (or (string? object)
      (symbol? object)))

;;;; Pathnames

;;@ Pathname disjoint type.
(define-record-type* pathname
  (really-make-pathname origin directory file)
  ())

(define (make-pathname origin directory file)
  (really-make-pathname origin
                        directory
                        (cond ((list? file)
                               (case (length file)
                                 ((0) (error 'make-pathname "empty list not allowed for file part"))
                                 ((1) (make-file (first file) #f))
                                 (else (make-file (first file) (cdr file)))))
                              ((string? file)
                               (make-file file #f))
                              (else file))))

;; Coerce OBJECT to a pathname.
;; If OBJECT is a symbol, return a pathname with a relative origin, an
;; empty directory, and a file whose name is the symbol.
;; If OBJECT is a string, parse it according to the optional file system
;; type FS-TYPE, which defaults to the local file system type.
;; If OBJECT is a pathname, return it.
;; any other case, signal an error.
(define/optional-args (x->pathname object (optional
                                           (fs-type (local-file-system-type))))
  (cond ((symbol?    object) (make-pathname #f '() object))
        ((string?    object) (parse-namestring object fs-type))
        ((os-string? object) (parse-namestring (os-string->string object)))
        ((pathname?  object) object)
        ((pair? object)
         (if (or (null? (car object)) (pair? (car object)))
             (make-pathname #f (car object)
                            (if (null? (cdr object))
                                #f
                                (make-file (cadr object) #f)))
             (make-pathname #f (drop-right object 1) (make-file (last object) #f))))
        (else (error 'x->pathname "cannot coerce to a pathname" object))))


;;;; Files

(define-record-type file
  (really-make-file name types version)
  file?
  (name file-name)
  (types file-types)
  (version file-version))

;;@ Make a pathname file with the given components. @1 is the file's
;; name, a string or a symbol.  @var{type} is the file's type or
;; types; it may be a string, a symbol, or a list of strings and
;; symbols. @var{version} is a non-negative integer representing the
;; file's version, or the symbol @code{newest} representing the most
;; recent version of the file.
(define/optional-args (make-file name type (optional (version #f)))
  (really-make-file name (make-file/type type) version))

(define (make-file/type type)
  (cond ((not type) '())
        ((string-or-symbol? type) (list type))
        ((and (list? type) (every string-or-symbol? type)) type)
        (else (error 'make-file "Invalid file type specifier: %S" type))))

;;@ Return the type of @1.
;; Return the last type if there is more than one.
;; Return @code{#f} if the file has no type.
(define (file-type file)
  (cond ((null? (file-types file)) #f)
        (else                      (last (file-types file)))))

;;@ Return @code{#t} if @1 is a valid file version and code{#f} if not.
(define (file-version? object)
  (or (not object)                     ; No version.
      (eq? object 'newest)
      (and (integer? object) (>= object 0))))

;;;; Pathname Component Substitution & Merging

;;@ Return a pathname like @1 with an origin of @2.
(define (pathname-with-origin pathname origin)
  (let ((pathname (x->pathname pathname)))
    (make-pathname origin
                   (pathname-directory pathname)
                   (pathname-file pathname))))

;;@ Return a pathname like @1 with a directory of @2.
(define (pathname-with-directory pathname directory)
  (let ((pathname (x->pathname pathname)))
    (make-pathname (pathname-origin pathname)
                   directory
                   (pathname-file pathname))))

;;@ Return a pathname like @1 with a file of @2.
(define (pathname-with-file pathname file)
  (let ((pathname (x->pathname pathname)))
    (make-pathname (pathname-origin pathname)
                   (pathname-directory pathname)
                   file)))

;;@ Return a pathname like @1.
;; Any null components of @1 are filled with the supplied
;; arguments, @1, @2 and @3.
(define (pathname-default pathname origin directory file)
  (let ((pathname (x->pathname pathname)))
    (make-pathname (or (pathname-origin pathname) origin)
                   (or (pathname-directory pathname) directory)
                   (or (pathname-file pathname) file))))

;; Return a pathname by merging PATHNAME with DEFAULTS-PATHNAME.
(define (merge-pathnames pathname defaults-pathname)
  (let* ((pathname (x->pathname pathname))
         (defaults-pathname (x->pathname defaults-pathname))
         (origin (pathname-origin pathname))
         (origin-default (pathname-origin defaults-pathname))
         (directory (pathname-directory pathname))
         (directory-default (pathname-directory defaults-pathname))
         (file (merge-files (pathname-file pathname)
                            (pathname-file defaults-pathname))))
    (if origin
        (make-pathname origin directory file)
        (make-pathname origin-default
                       (cond ((not directory) directory-default)
                             ((not directory-default) directory)
                             (else (append directory-default directory)))
                       file))))

;;@ Return a file by merging @1 with @2.
(define (merge-files file defaults-file)
  (cond ((not file)          defaults-file)
        ((not defaults-file) file)
        (else
         (let ((name (file-name file))
               (defaults-name (file-name defaults-file))
               (types (file-types file))
               (defaults-types (file-types defaults-file))
               (version (file-version file))
               (defaults-version (file-version defaults-file)))
           (make-file (or name defaults-name)
                      (if (null? types) defaults-types types)
                      (or version defaults-version))))))

;;@ Return a pathname whose merging with RELATIVE produces PATHNAME.
;; This is currently unimplemented and will simply return PATHNAME."
(define (enough-pathname pathname relative)
  (error 'enough-pathname "Unimplemented: %S"
         `(enough-pathname ',pathname ',relative)))

;;;; Directory Pathnames

;;@ Returns @code{#t} if @1 has directory components but no file,
;; and @code{#f} if otherwise."
(define (directory-pathname? pathname)
  (let ((pathname (x->pathname pathname)))
    (and (pathname-directory pathname)  ;++ nil/false pun
         (not (pathname-file pathname)))))

;;@ Return a pathname like @1, representing a directory.
;; If @1 has a file component, it is added to the end of the list of
;; directory components, and the resultant pathname has no file.
;; Otherwise, return @1.
(define (pathname-as-directory pathname)
  (let* ((pathname (x->pathname pathname))
         (file (pathname-file pathname)))
    (if file
        (make-pathname (pathname-origin pathname)
                       (let ((directory (pathname-directory pathname)))
                         (if directory
                             (append directory (list (file-namestring pathname)))
                             (list (file-namestring pathname))))
                       #f)
        pathname)))

;;@ Return a pathname of the directory that contains @1.
(define (pathname-container pathname)
  (let* ((pathname (x->pathname pathname))
         (origin (pathname-origin pathname))
         (directory (pathname-directory pathname))
         (file (pathname-file pathname)))
    (let loop ((pathname pathname))
      (cond (file
             (make-pathname origin directory #f))
            ((and directory (not (null? directory)))
             (make-pathname origin (drop-right directory 1) #f))
            (else
             (let ((expansion (expand-pathname pathname)))
               (if (equal? expansion pathname)
                   (error 'pathname-container
                          "Unable to find pathname's container: %S"
                          pathname)
                   (loop expansion))))))))

;;@ Return a pathname by interpreting @2 as a series of directories
;; relative to the origin and directory of @1, with the last element
;; of @2 specifying the file component of the result.
(define (pathname-join base . steps)
  (let ((base (x->pathname base)))
    (if (null? steps)
        base
        (let loop ((steps steps) (dirs (list (pathname-directory base))) (file #f))
          (if (null? steps)
              (make-pathname (pathname-origin base)
                             (concatenate (reverse dirs))
                             file)
              (let ((step (x->pathname (car steps))))
                (if (null? (cdr steps))
                    (loop (cdr steps)
                          (cons (pathname-directory step) dirs)
                          (pathname-file step))
                    (loop (cdr steps)
                          (cons (pathname-directory (pathname-as-directory step)))
                          file))))))))


;;;; Pathname Expansion

;;@ Return a pathname like @1 but with the origin expanded.
;; This is currently unimplemented and will simply return @1."
(define (expand-pathname pathname)
  (error 'expand-pathname "Unimplemented: %S" `(expand-pathname ',pathname)))

;;@ Compare the pathnames @1 and @2 and return @code{#t} if they refer
;; to the same filesystem entity.
;;
;; FIXME: doesn't deal with versions.
(define (pathname=? x y)
  (and (equal? (pathname-origin x) (pathname-origin y))
       (equal? (pathname-directory x) (pathname-directory y))
       (or (and (eqv? (pathname-file x) #f)
                (eqv? (pathname-file y) #f))
           (and (pathname-file x) (pathname-file y)
                (equal? (file-name (pathname-file x))
                        (file-name (pathname-file y)))
                (equal? (file-types (pathname-file x))
                        (file-types (pathname-file y)))))))

(define (%->string obj)
  (cond ((string? obj) obj)
        ((symbol? obj) (symbol->string obj))
        (else (error 'pathname-compare "cannot coerce to string" obj))))

(define (make-pathname-file-comp pred)
  (lambda (x y)
    (let ((file-x (pathname-file x))
          (file-y (pathname-file y)))
      (cond ((pathname=? x y) #f)
            ((and file-x (file-name file-x)
                  file-y (file-name file-y)
                  (equal? (pathname-origin x)
                          (pathname-origin y))
                  (equal? (map %->string (pathname-directory x))
                          (map %->string (pathname-directory y))))
             (pred (%->string (file-name file-x)) (%->string (file-name file-y))))
            (else
             #f)))))
;;@stop

(define pathname<? (make-pathname-file-comp string<?))
(define pathname>? (make-pathname-file-comp string>?))


;;;; Namestrings

;;@ Parse @1 and return a pathname representing it.
;; Use @2's namestring parser to parse @1.
;; If @2 is not supplied, it defaults to the local file system."
(define/optional-args (parse-namestring namestring
                                        (optional (fs-type (local-file-system-type))))
  (fs-type/parse-namestring fs-type namestring))

;;@ Coerce @1 into a namestring.
;; If @1 is a string, canonicalize it according to @2.
;; If @1 is a pathname, convert it according to @2.
;; Otherwise, signal an error.
;; If @2 is not supplied, it defaults to the local file system type."
(define/optional-args (x->namestring object (optional
                                             (fs-type (local-file-system-type))))
  ;++ What if it's a symbol?  Use (MAKE-PATHNAME NIL NIL object)?
  (cond ((string? object)
         (fs-type/canonicalize-namestring fs-type object))
        ((pathname? object)
         (pathname->namestring object fs-type))
        ((pair? object)
         (pathname->namestring (x->pathname object) fs-type))
        (else (error 'x->namestring "Unable to coerce to a namestring: %S" object))))

(define (pathname->namestring pathname fs-type)
  (fs-type/pathname->namestring fs-type pathname))

;;@ Return a string for @1's origin according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define/optional-args (origin-namestring pathname
                                         (optional (fs-type (local-file-system-type))))
  (fs-type/origin-namestring fs-type (x->pathname pathname)))

;; Return a string for @1's directory according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define/optional-args (directory-namestring
                       pathname (optional (fs-type (local-file-system-type))))
  (fs-type/directory-namestring fs-type (x->pathname pathname)))

;; Return a string for @1's file according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define/optional-args (file-namestring pathname (optional
                                                 (fs-type (local-file-system-type))))
  (fs-type/file-namestring fs-type (x->pathname pathname)))

;; Return a string naming @1 relative to RELATIVE,
;; according to @2.
;; If @2 is not supplied, it defaults to the local file system type."
(define/optional-args (enough-namestring pathname relative
                                         (optional (fs-type (local-file-system-type))))
  (fs-type/enough-namestring fs-type
                             (pathname (x->pathname pathname))
                             (x->pathname relative)))

;;;; File System Types

(define-operation (fs-type/parse-namestring fs-type namestring))

(define-operation (fs-type/enough-namestring fs-type pathname relative)
  (x->namestring (enough-pathname pathname relative) fs-type))

(define-operation (fs-type/canonicalize-namestring fs-type object)
  (pathname->namestring (x->pathname object) fs-type))

(define-operation (fs-type/origin-namestring fs-type pathname))
(define-operation (fs-type/directory-namestring fs-type pathname))
(define-operation (fs-type/file-namestring fs-type pathname))

(define-operation (fs-type/pathname->namestring fs-type pathname)
  (string-append (origin-namestring pathname)
                 (directory-namestring pathname)
                 (file-namestring pathname)))

(define-operation (fs-type/parse-file-namestring fs-type namestring)
  (make-file namestring '() #f))

(define (string-split s c start)
  (string-tokenize s (char-set-complement (char-set c))))

(define unix-file-system-type
  (object #f
    ((fs-type/origin-namestring self pathname)
     (let ((origin (pathname-origin pathname)))
       (cond ((eqv? origin #f) "")
             ((or (eq? origin '/) (equal? origin "/")) "/")
             (else
              (error 'unix/origin-namestring "invalid origin for unix file system" origin)))))
    
    ((fs-type/directory-namestring self pathname)
     (let ((dir (pathname-directory pathname)))
       (if (null? dir)
           "."
           (string-append (string-join (map %->string dir) "/") "/"))))

    ((fs-type/pathname->namestring self pathname)
     
     (string-append (fs-type/origin-namestring self pathname)
                    (cond ((and (null? (pathname-directory pathname))
                                (not (pathname-file pathname)))
                           
                           ".")
                          ((null? (pathname-directory pathname))
                           "")
                          (else
                           (fs-type/directory-namestring self pathname)))
                    (fs-type/file-namestring self pathname)))
    
    ((fs-type/file-namestring self pathname)
     (let ((file (pathname-file pathname)))
       (if (not file)
           ""
           (string-concatenate
            (cons (file-name file)
                  (if (null? (file-types file))
                      '()
                      (list "." (string-join (file-types file) "."))))))))
    
    ((fs-type/parse-namestring self namestring)
     (let ((parts (remove string-null? (string-split namestring #\/ 0)))
           (absolute? (string-prefix? "/" namestring))
           (directory? (string-suffix? "/" namestring)))
       (make-pathname
        (if absolute? '/ #f)
        (if directory? parts (drop-right parts 1))
        (if directory?
            #f
            (let ((file-part (last parts)))
              (fs-type/parse-file-namestring self file-part))))))

    ((fs-type/parse-file-namestring self namestring)
     (if (parse-unix-file-types)
         (receive (prefix file-parts)
                  (cond ((string-every #\. namestring)
                         (values "" (list namestring)))
                        ((string-skip namestring #\.)
                         => (lambda (idx)
                              (values (substring/shared namestring 0 idx)
                                      (string-split namestring #\. idx))))
                        (else (values "" (string-split namestring #\. 0))))
           (if (and (null? file-parts) (string-null? prefix))
               #f
               (make-file (string-append prefix (first file-parts))
                          (cdr file-parts))))
         (make-file namestring '())))))

(define parse-unix-file-types (make-parameter #t))

(define (local-file-system-type)
  unix-file-system-type)

;; these go last, since it may expand to an expression, not a definition
(define-record-discloser pathname
  (lambda (pathname)
    (list 'pathname
          (pathname-origin pathname)
          (pathname-directory pathname)
          (pathname-file pathname))))

(define-record-discloser file
  (lambda (file)
    (list 'file (file-name file) (file-types file) (file-version file))))

