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
                #:enqueue-to-queue)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:enqueue
           #:enqueue-to
           #:dequeue
           #:all-queues
           #:queue-size
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
  (let ((ret
          (with-connection *connection*
            (apply #'red:blpop
                   (nconc
                    (mapcar (lambda (queue)
                              (redis-key "queue" queue))
                            (ensure-list queue-or-queues))
                    (list timeout))))))
    (if ret
        (destructuring-bind (queue payload) ret
          (values payload (omit-redis-prefix queue)))
        nil)))

(defun all-queues ()
  (with-connection *connection*
    (red:smembers (redis-key "queues"))))

(defun queue-size (queue)
  (with-connection *connection*
    (red:llen (redis-key "queue" queue))))

(defun queue-empty-p (queue)
  (zerop (queue-size queue)))

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
