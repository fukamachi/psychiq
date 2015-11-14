(in-package :cl-user)
(defpackage redqing-test.connection
  (:use #:cl
        #:prove
        #:redqing.connection))
(in-package :redqing-test.connection)

(plan 2)

(subtest "connect, disconnect & reconnect"
  (let ((conn (connect :host "localhost" :port 6379)))
    (is-type conn 'connection
             "CONNECT returns 'CONNECTION'")
    (ok (connectedp conn)
        "Connection is open")

    (is-type (reconnect conn) 'connection
             "RECONNECT returns 'CONNECTION'")
    (ok (connectedp conn)
        "Connection is open")

    (is-type (disconnect conn) 'connection
             "DISCONNECT returns 'CONNECTION'")
    (ok (not (connectedp conn))
        "Connection is closed")
    (is (disconnect conn) nil
        "Don't raise an error when closing a closed connection")

    (ok (connectedp (reconnect conn))
        "Can RECONNECT a disconnected connection")

    (disconnect conn)))

(subtest "print-object"
  (let ((conn (connect :host "localhost" :port 6379)))
    (is (princ-to-string conn)
        "#<CONNECTION localhost:6379>")
    (disconnect conn)))

(finalize)
