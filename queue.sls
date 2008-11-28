;;; queue.sls --- Functional queues.

;; Adapted from the Schematics Cookbook,
;; http://schemecookbook.org/view/Cookbook/FunctionalQueue;
;; as all content there, licensed under the GNU LGPL v2.1.

;;; Commentary:

;;; Code:


(library (spells queue)
  (export queue? empty-queue queue-empty? queue-insert queue-remove queue-first)

  (import (rnrs base)
          (rnrs records syntactic))
  
  (define-record-type queue
    (fields front back))

  (define empty-queue
    (let ((empty (make-queue '() '())))
      (lambda () empty)))
  
  ;;@ Return @code{#t} if the queue @1 is empty, @code{#f} otherwise.
  (define (queue-empty? Q) ; queue -> boolean
    (and (null? (queue-front Q))
         (null? (queue-back Q))))
  
  ;;@ Return a new queue holding both the elements
  ;;  from the old queue @1 and the new element @2.
  (define (queue-insert Q x) ; queue elm -> queue
    (make-queue (queue-front Q) 
                (cons x (queue-back Q))))

  ;;@ Return two values, the first a queue with the same elements
  ;;  as the queue @1 except for the element in front of the queue
  ;;  the second a value is the front element of @1.
  (define (queue-remove Q) ; queue -> (values queue elm)
    (cond
     ((and (null? (queue-front Q)) (null? (queue-back Q)))
      (error "remove: The queue is empty"))
     ((null? (queue-front Q))
      (queue-remove (make-queue (reverse (queue-back Q)) '())))
     (else
      (values (car (queue-front Q)) 
              (make-queue (cdr (queue-front Q)) (queue-back Q))))))

  ;;@ Return the first element in the non-empty queue @1.
  (define (queue-first Q) ; queue -> element
    (cond
     ((queue-empty? Q)
      (error "first: The queue is empty"))
     ((null? (queue-front Q))
      (queue-first (make-queue (reverse (queue-back Q)) '())))
     (else
      (car (queue-front Q)))))
)