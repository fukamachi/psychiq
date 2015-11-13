(in-package :cl-user)
(defpackage redqing.queue
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:connection-coder
                #:with-redis-connection)
  (:import-from #:redqing.coder
                #:encode)
  (:import-from #:redqing.redis
                #:with-transaction
                #:redis-key)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf)
  (:export #:enqueue-to-queue))
(in-package :redqing.queue)

(defun enqueue-to-queue (conn queue payload)
  (check-type conn connection)
  (labels ((encode-payload (payload)
             (encode (connection-coder conn)
                     payload)))
    (with-redis-connection conn
      (with-transaction
        (nconcf payload `(("enqueued_at" . ,(timestamp-to-unix (now)))))
        (red:sadd (redis-key "queues") queue)
        (red:rpush (redis-key "queue" queue)
                   (encode-payload payload))))))
