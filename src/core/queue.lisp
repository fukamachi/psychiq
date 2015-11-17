(in-package :cl-user)
(defpackage redqing.queue
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:with-redis-connection)
  (:import-from #:redqing.coder
                #:encode-object)
  (:import-from #:redqing.redis
                #:with-transaction
                #:redis-key)
  (:import-from #:redqing.util.assoc
                #:aget)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:enqueue-to-queue))
(in-package :redqing.queue)

(defun enqueue-to-queue (conn queue job-info)
  (check-type conn connection)
  (with-redis-connection conn
    (with-transaction
      (setf (aget job-info "enqueued_at") (timestamp-to-unix (now)))
      (red:sadd (redis-key "queues") queue)
      (red:rpush (redis-key "queue" queue)
                 (encode-object job-info))))
  job-info)
