(in-package :cl-user)
(defpackage psychiq-test.queue
  (:use #:cl
        #:prove
        #:psychiq.connection
        #:psychiq.queue))
(in-package :psychiq-test.queue)

(plan 1)

(let ((conn (connect :host "localhost" :port 6379)))
  (unwind-protect
       (progn
         (with-connection conn
           (red:del "psychiq:queue:test")
           (ok (enqueue-to-queue
                "test"
                `(("class" . "test-job")
                  ("args" . ("12"))
                  ("jid" . "9hrm7ofvc44u")))
               "Can enqueue")))
    (disconnect conn)))

(finalize)
