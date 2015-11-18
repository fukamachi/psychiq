(in-package :cl-user)
(defpackage redqing.queue
  (:use #:cl
        #:redqing.util)
  (:import-from #:redqing.coder
                #:encode-object)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:enqueue-to-queue))
(in-package :redqing.queue)

(defun enqueue-to-queue (queue job-info)
  (with-redis-transaction
    (setf (aget job-info "enqueued_at") (timestamp-to-unix (now)))
    (red:sadd (redis-key "queues") queue)
    (red:rpush (redis-key "queue" queue)
               (encode-object job-info)))
  job-info)
