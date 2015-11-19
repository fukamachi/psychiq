(in-package :cl-user)
(defpackage redqing.client
  (:use #:cl
        #:redqing.util
        #:redqing.specials)
  (:import-from #:redqing.connection
                #:with-connection
                #:*connection*)
  (:import-from #:redqing.job
                #:encode-job)
  (:import-from #:redqing.coder
                #:decode-object)
  (:import-from #:redqing.queue
                #:enqueue-to-queue
                #:dequeue-from-queue)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:enqueue
           #:enqueue-to
           #:dequeue
           #:all-queues
           #:queue-length
           #:queue-empty-p
           #:delete-queue
           #:slice-queue
           #:peek-queue))
(in-package :redqing.client)

(defgeneric enqueue (job-class &optional args))

(defmethod enqueue ((job-class symbol) &optional args)
  (enqueue-to *default-queue-name* job-class args))

(defun enqueue-to (queue job-class &optional args)
  (let ((job-info (encode-job (make-instance job-class) args)))
    (with-connection *connection*
      (enqueue-to-queue queue job-info))))

(defun dequeue (&optional
                  (queue-or-queues *default-queue-name*)
                  (timeout 5))
  (with-connection *connection*
    (dequeue-from-queue queue-or-queues :timeout timeout)))

(defun all-queues ()
  (with-connection *connection*
    (red:smembers (redis-key "queues"))))

(defun queue-length (queue)
  (with-connection *connection*
    (red:llen (redis-key "queue" queue))))

(defun queue-empty-p (queue)
  (zerop (queue-length queue)))

(defun delete-queue (queue)
  (with-connection *connection*
    (redis:with-pipelining
      (red:del (redis-key "queue" queue))
      (red:srem (redis-key "queues") queue)))
  (values))

(defun slice-queue (queue start &optional (end -1))
  (mapl (lambda (jobs)
          (rplaca jobs (decode-object (first jobs))))
        (with-connection *connection*
          (red:lrange (redis-key "queue" queue) start end))))

(defun peek-queue (queue &optional (start 0) (count 1))
  (first (slice-queue queue start (1- count))))
