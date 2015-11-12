(in-package :cl-user)
(defpackage redqing-test.connection
  (:use #:cl
        #:prove
        #:redqing.connection))
(in-package :redqing-test.connection)

(plan 1)

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
        "Connection is closed")))

(finalize)

