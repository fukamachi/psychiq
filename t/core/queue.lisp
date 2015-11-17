(in-package :cl-user)
(defpackage redqing-test.queue
  (:use #:cl
        #:prove
        #:redqing.connection
        #:redqing.queue))
(in-package :redqing-test.queue)

(plan 1)

(let ((conn (connect :host "localhost" :port 6379)))
  (unwind-protect
       (progn
         (with-redis-connection conn
           (red:del "redqing:queue:test"))
         (ok (enqueue-to-queue conn
                               "test"
                               `(("class" . "test-job")
                                 ("args" . ("12"))
                                 ("jid" . "9hrm7ofvc44u")))
             "Can enqueue"))
    (disconnect conn)))

(finalize)
