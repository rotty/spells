#!r6rs

(library (spells record-types expand-drt)
  (export expand-define-record-type*
          expand-define-functional-fields)
  (import (rnrs)
          (only (srfi :1 lists) append-map)
          (srfi :8 receive)
          (spells parameter))

;;;;;; Alternative record type definition macro

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (expand-define-record-type* form rename compare)
  ((call-with-current-continuation
    (lambda (lose)
      (lambda ()
        (parameterize (($lose (lambda (message subform)
                                (lose
                                 (lambda ()
                                   (syntax-violation message form subform))))))
          (let ((type-name (cadr form))
                (conser-name (caaddr form))
                (conser-args (cdaddr form))
                (other-fields (cadddr form)))
            (receive (needs-conser-layer? arg-tags vars inits)
                (compute-vars+inits conser-args other-fields)
              (let ((real-conser
                     (if needs-conser-layer?
                         (rename (symbol-append '% conser-name))
                         conser-name)))
                `(,(rename 'begin)
                  (,(rename 'define-record-type) ,type-name
                   (,real-conser ,@arg-tags)
                   ,(symbol-append type-name '?)
                   ,@(generate-field-specs conser-args
                                           other-fields
                                           type-name))
                  ,@(if needs-conser-layer?
                        `((,(rename 'define) (,conser-name ,@vars)
                           (,real-conser ,@inits)))
                        '())))))))))))

(define $lose (make-parameter #f))
(define (lose msg subform) (($lose) msg subform))

(define (compute-vars+inits conser-args other-fields)
  (let ((vars (reverse-map
               (lambda (x)
                 (cond ((symbol? x) x)
                       ((and (pair? x)
                             (symbol? (car x))
                             (null? (cdr x)))
                        (car x))
                       (else (lose "invalid maker argument specifier" x))))
               conser-args)))
    (let loop ((fields other-fields)
               (needs-conser-layer? #f)
               (arg-tags vars)
               (inits vars))
      (if (null? fields)
          (values needs-conser-layer?
                  (reverse arg-tags)
                  (reverse vars)
                  (reverse inits))
          (let ((field (car fields)))
            (cond ((symbol? field)
                   (loop (cdr fields)
                         needs-conser-layer?
                         arg-tags
                         inits))
                  ((and (pair? field)
                        (symbol? (car field))
                        (pair? (cdr field))
                        (null? (cddr field)))
                   (loop (cdr fields)
                         #t
                         (cons (car field) arg-tags)
                         (cons (cadr field) inits)))
                  (else
                   (lose "invalid field specifier" field))))))))

(define (reverse-map proc list)
  (let loop ((list list) (tail '()))
    (if (null? list)
        tail
        (loop (cdr list) (cons (proc (car list)) tail)))))

(define (generate-field-specs conser-args other-fields type-name)
  (append (map (lambda (x)
                 (receive (tag set?)
                          (if (pair? x)
                              (values (car x) #t)
                              (values x #f))
                   `(,tag ,(make-field-accessor type-name
                                                tag)
                          ,@(if set?
                                (list (make-field-setter
                                       type-name
                                       tag))
                                '()))))
               conser-args)
          (map (lambda (x)
                 (let ((tag (if (pair? x) (car x) x)))
                   `(,tag ,(make-field-accessor type-name tag)
                          ,(make-field-setter   type-name tag))))
               other-fields)))

(define (make-field-accessor type-name tag)
  (symbol-append type-name '- tag))

(define (make-field-setter type-name tag)
  (symbol-append 'set- type-name '- tag '!))

(define (make-field-modifier type-name tag)
  (symbol-append type-name "-modify-" tag))

(define (make-field-replacer type-name tag)
  (symbol-append type-name "-with-" tag))

(define (make-field-default type-name tag)
  (symbol-append type-name "-default-" tag))

(define (symbol-append . symbols)
  (string->symbol (apply string-append
                         (map (lambda (x)
                                (cond ((string? x) x)
                                      (else (symbol->string x))))
                              symbols))))

(define (expand-define-functional-fields form r compare)
  (let ((type-name (cadr form))
        (fields (cddr form)))
    (let ((obj (r 'obj))
          (value (r 'value))
          (modifier (r 'modifier))
          (unconser (symbol-append type-name "-components"))
          (conser (symbol-append "make-" type-name)))
      `(,(r 'begin)
        (,(r 'define) (,unconser ,obj)
         (,(r 'values) ,@(map (lambda (f)
                                `(,(make-field-accessor type-name f) ,obj))
                              fields)))
        ,@(append-map
           (lambda (field)
             `((,(r 'define) (,(make-field-replacer type-name field) ,obj ,value)
                (,(r 'receive) ,fields (,unconser ,obj)
                 (,conser ,@(map (lambda (f) (if (eq? f field) value f)) fields))))
               (,(r 'define) (,(make-field-modifier type-name field) ,obj ,modifier)
                (,(make-field-replacer type-name field)
                 ,obj (,modifier (,(make-field-accessor type-name field) ,obj))))
               (,(r 'define) (,(make-field-default type-name field) ,obj ,value)
                (,(make-field-modifier type-name field) ,obj (,(r 'lambda) (v)
                                                              (,(r 'or) v ,value))))))
           fields)))))

)
