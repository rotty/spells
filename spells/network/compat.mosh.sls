#!r6rs
; compat.mosh.sls - (spells network) compatibility layer for Mosh
; copyright: 2011 David Banks <amoebae@gmail.com>; license: BSD-3-clause

(library (spells network compat)
  (export connection?
          connection-input-port
          connection-output-port
          connection-socket
          close-connection

          listener?
          listener-accept
          listener-address
          close-listener

          open-tcp-connection
          open-tcp-listener)

  (import (rnrs)
          (prefix (mosh socket) mosh:)
          (spells network utils))

  (define-record-type connection
    (protocol (lambda (p)
                (lambda (socket)
                  (p
                   socket
                   (mosh:socket-port socket)
                   (mosh:socket-port socket)))))
    (fields socket input-port output-port))

  (define (close-connection conn)
    (mosh:socket-close (connection-socket conn)))

  (define-record-type listener
    (fields socket))

  (define (listener-accept listener)
    (make-connection (mosh:socket-accept (listener-socket listener))))

  (define (close-listener listener)
    (mosh:socket-close (listener-socket listener)))

  (define (listener-address listener)
    ; Mosh doesn't support this yet
    #f)

  (define (open-tcp-connection address service)
    (make-connection
     (mosh:make-client-socket
      address
      (cond ((integer? service) (number->string service))
            ((symbol? service)  (symbol->string service))
            (else               service)))))

  (define (open-tcp-listener . maybe-options)
    (let-options* (if (null? maybe-options) '() (car maybe-options))
     ((service #f))
     (unless service
       (raise-impl-restriction 'open-tcp-listener
                               "ephemeral ports not supported"))
     (make-listener
      (mosh:make-server-socket (number->string service)))))
)
