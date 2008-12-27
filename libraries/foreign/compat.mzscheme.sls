;;;(library (spells foreign compat) (export) (import))
#lang scheme

(provide make-pointer-c-getter make-pointer-c-setter

         pointer? null-pointer null-pointer?
         pointer=?
         pointer+

         make-c-callout
         make-c-callback

         (rename-out (spells:malloc malloc)
                     (spells:memcpy memcpy)
                     (spells:memset memset))
         free

         dlopen dlsym dlclose dlerror)

(require rnrs/base-6
         rnrs/control-6
         rnrs/arithmetic/bitwise-6
         rnrs/lists-6
         rnrs/bytevectors-6
         (lib "xitomatl/srfi/and-let%2a.sls")
         (lib "spells/alist.sls")
         (lib "spells/parameter.sls")
         (lib "spells/foreign/config.sls")
         (lib "spells/tracing.sls")
         scheme/mpair
         scheme/foreign
         (only-in '#%foreign ffi-callback))

(unsafe!)

(define (sized-type ctype signed?)
  (case (c-type-sizeof ctype)
    ((1)  (if signed? 'int8 'uint8))
    ((2)  (if signed? 'int16 'uint16))
    ((4)  (if signed? 'int32 'uint32))
    ((8)  (if signed? 'int64 'uint64))
    (else
     (assertion-violation 'c-type-aliases
                          "unexpected return value from c-type-sizeof"
                          ctype))))

(define (sized-types-aliases)
  (map (lambda (ctype)
         (let ((signed? (memq ctype '(char short int long llong))))
           (cons (sized-type ctype signed?) ctype)))
       '(char uchar short ushort int uint long ulong llong ullong)))

(define (other-types-aliases)
  `((size_t . ,(sized-type 'size_t #f))
    (ssize_t . ,(sized-type 'ssize_t #t))
    ;; we assume time_t to be a signed integer type; this true at
    ;; least on glibc systems
    (time_t . ,(sized-type 'time_t #t))))

(define c-type-aliases (append (sized-types-aliases) (other-types-aliases)))

(define (resolve-alias ctype)
  (cond ((assq-ref c-type-aliases ctype)
         => (lambda (alias)
              (or (resolve-alias alias)
                  alias)))
        (else #f)))

(define (make-pointer-c-getter sym)
  (let ((mz-type (type->mz-type sym)))
    (or (and mz-type (lambda (ptr offset)
                       (ptr-ref (ptr-add ptr offset) mz-type)))
        (error 'make-pointer-c-getter "invalid type" sym))))

(define (pointer? thing)
  (cpointer? thing))

(define (null-pointer)
  #f)

(define (null-pointer? thing)
  (eqv? thing #f))

(define (pointer=? p1 p2)
  (ptr-equal? p1 p2))

(define (pointer+ p offset)
  (ptr-add p offset))

(define (spells:malloc n-bytes)
  (malloc n-bytes 'raw))

(define (make-c-callout ret-type arg-types)
  (let ((ctype (_cprocedure (mlist->list (map type->mz-type arg-types))
                            (type->mz-type ret-type))))
    (lambda (ptr)
      (function-ptr ptr ctype))))

(define (make-c-callback ret-type arg-types)
  (let ((mz-arg-types (mlist->list (map type->mz-type arg-types)))
        (mz-ret-type (type->mz-type ret-type)))
    (lambda (proc)
      (ffi-callback proc mz-arg-types mz-ret-type))))

(define (type->mz-type type)
  (define (prim->mz-type prim)
    (case prim
      ((char) _int8)
      ((uchar) _uint8)
      ((short) _sshort)
      ((ushort) _ushort)
      ((int) _sint)
      ((uint) _uint)
      ((long) _slong)
      ((ulong) _ulong)
      ((float) _float)
      ((double) _double)
      ((pointer) _pointer)
      ((fpointer) _fpointer)
      ((void) _void)
      (else #f)))
  (or (prim->mz-type type)
      (and-let* ((alias (resolve-alias type)))
        (prim->mz-type alias))
      (error 'type->mz-type "invalid type" type)))

(define (make-pointer-c-setter sym)
  (let ((mz-type (type->mz-type sym)))
    (or (and mz-type (lambda (ptr offset val)
                       (ptr-set! (ptr-add ptr offset) mz-type val)))
        (error 'make-pointer-c-setter "invalid type" sym))))

(define (spells:memcpy p . args)
  (apply memcpy p args)
  p)

(define (spells:memset p v n)
  (memset p v n)
  p)

(define *dlerror* #f)

(define dlopen
  (case-lambda
    ((lib-name lazy? global?)
     (with-handlers ((exn:fail? (lambda (exn)
                                  (set! *dlerror* exn)
                                  #f)))
       (let ((result (ffi-lib lib-name)))
         (set! *dlerror* #f)
         result)))
    ((lib-name)
     (dlopen lib-name #f #f))
    (()
     (dlopen #f #f #f))))

(define (dlsym lib-ptr str)
  (get-ffi-obj str lib-ptr _fpointer
               (lambda () #f)))

;; Not possible in PLT
(define (dlclose lib-ptr)
  #f)

(define (dlerror)
  *dlerror*)
