;;; network.sls --- Network interface

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells network)
  (export connection?
          connection-input-port
          connection-output-port
          close-connection
          
          listener?
          listener-address
          listener-accept
          close-listener
          
          listener-accept-loop
          
          open-tcp-listener)
  (import (rnrs)
          (srfi :8 receive)
          (spells network compat))

(define (listener-accept-loop open initializer receiver . seeds)
  (let ((listener #f))
    (dynamic-wind
      (lambda () (set! listener (open)))
      (lambda ()
        (initializer (listener-address listener))
        (let loop ((seeds seeds))
          (let ((connection (listener-accept listener)))
            (receive (continue? . seeds) (apply receiver connection seeds)
              (close-connection connection)
              (cond (continue?
                     (loop seeds))
                    (else
                     (apply values seeds)))))))
      (lambda () (close-listener listener)))))

)
