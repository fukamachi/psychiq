(in-package :cl-user)
(defpackage redqing.queue
  (:use #:cl
        #:redqing.util)
  (:import-from #:redqing.coder
                #:encode-object
                #:decode-object)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:enqueue-to-queue
           #:dequeue-from-queue))
(in-package :redqing.queue)

(defun enqueue-to-queue (queue job-info)
  (with-redis-transaction
    (setf (aget job-info "enqueued_at") (timestamp-to-unix (now)))
    (red:sadd (redis-key "queues") queue)
    (red:rpush (redis-key "queue" queue)
               (encode-object job-info)))
  job-info)

(defun dequeue-from-queue (queue-or-queues &key (timeout 5))
  (let ((ret
          (apply #'red:blpop
                 (nconc
                  (mapcar (lambda (queue)
                            (redis-key "queue" queue))
                          (ensure-list queue-or-queues))
                  (list timeout)))))
    (if ret
        (destructuring-bind (queue payload) ret
          (values (decode-object payload)
                  (omit-redis-prefix queue "queue")))
        nil)))
