(in-package :cl-user)
(defpackage psychiq.client
  (:use #:cl
        #:psychiq.util
        #:psychiq.specials)
  (:import-from #:psychiq.connection
                #:with-connection
                #:*connection*)
  (:import-from #:psychiq.worker
                #:queue-name
                #:encode-job)
  (:import-from #:psychiq.coder
                #:decode-object)
  (:import-from #:psychiq.queue
                #:enqueue-to-queue
                #:dequeue-from-queue)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:enqueue
           #:dequeue
           #:all-queues
           #:queue-length
           #:queue-empty-p
           #:delete-queue
           #:slice-queue
           #:peek-queue
           #:all-retries
           #:retry-length
           #:all-dead-jobs))
(in-package :psychiq.client)

(defun enqueue (worker-class &optional args)
  (let ((job-info (encode-job worker-class args))
        (queue (queue-name
                (allocate-instance (find-class worker-class)))))
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

(defun decode-objects (objects)
  (mapl (lambda (objects)
          (rplaca objects (decode-object (first objects))))
        objects))

(defun slice-queue (queue start &optional (end -1))
  (decode-objects
   (with-connection *connection*
     (red:lrange (redis-key "queue" queue) start end))))

(defun peek-queue (queue &optional (start 0) (count 1))
  (first (slice-queue queue start (1- count))))

(defun all-retries ()
  (decode-objects
   (with-connection *connection*
     (red:zrange (redis-key "retry") 0 -1))))

(defun retry-length ()
  (with-connection *connection*
    (red:zcard (redis-key "retry"))))

(defun all-dead-jobs ()
  (decode-objects
   (with-connection *connection*
     (red:zrange (redis-key "dead") 0 -1))))
