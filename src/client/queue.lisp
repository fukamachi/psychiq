(in-package :cl-user)
(defpackage redqing.queue
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:with-redis-connection)
  (:export #:enqueue-to-queue))
(in-package :redqing.queue)

(defun enqueue-to-queue (conn queue payload)
  (check-type conn connection)
  (with-redis-connection conn
    (red:rpush (format nil "redqing:queue:~A" queue)
               payload)))
