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
                #:encode-object
                #:decode-object)
  (:import-from #:psychiq.queue
                #:enqueue-to-queue
                #:enqueue-to-scheduled-queue
                #:dequeue-from-queue)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:timestamp+
                #:now)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:enqueue
           #:enqueue-bulk
           #:enqueue-in-sec
           #:dequeue
           #:all-queues
           #:queue-length
           #:queue-empty-p
           #:delete-queue
           #:slice-queue
           #:peek-queue
           #:all-retries
           #:retry-length
           #:all-dead-jobs
           #:stats))
(in-package :psychiq.client)

(defun enqueue (worker-class &optional args)
  (let ((job-info (encode-job worker-class args))
        (queue (queue-name
                (allocate-instance (find-class worker-class)))))
    (with-connection *connection*
      (enqueue-to-queue queue job-info))))

(defun enqueue-bulk (worker-class job-args &rest more-job-args)
  (let ((queue (queue-name
                (allocate-instance (find-class worker-class))))
        (now (timestamp-to-unix (now)))
        (jobs '()))
    (with-connection *connection*
      (with-redis-transaction
        (dolist (args (cons job-args more-job-args))
          (let ((job-info (encode-job worker-class args)))
            (setf (aget job-info "enqueued_at") now)
            (red:rpush (redis-key "queue" queue)
                       (encode-object job-info))
            (push job-info jobs)))
        (red:sadd (redis-key "queues") queue)))
    (nreverse jobs)))

(defun enqueue-in-sec (interval worker-class &optional args)
  (check-type interval fixnum)
  (assert (< 0 interval))
  (let ((job-info (encode-job worker-class args))
        (queue (queue-name
                (allocate-instance (find-class worker-class)))))
    (setf (aget job-info "queue") queue)
    (with-connection *connection*
      (enqueue-to-scheduled-queue job-info
                                  (timestamp-to-unix (timestamp+ (now) interval :sec))))))

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

(defun stats ()
  (with-connection *connection*
    (destructuring-bind (processed failed
                         schedule retry dead
                         default-queue-last-job
                         queues)
        (redis:with-pipelining
          (red:get (redis-key "stat" "processed"))
          (red:get (redis-key "stat" "failed"))
          (red:zcard (redis-key "schedule"))
          (red:zcard (redis-key "retry"))
          (red:zcard (redis-key "dead"))
          (red:lrange (redis-key "queue" "default") -1 -1)
          (red:smembers (redis-key "queues")))
      `(:processed ,(parse-integer processed)
        :failed    ,(parse-integer failed)
        :scheduled ,schedule
        :retry ,retry
        :dead ,dead
        :default-queue-latency ,(if default-queue-last-job
                                    (- (timestamp-to-unix (now))
                                        (aget (decode-object
                                               (first default-queue-last-job))
                                              "enqueued_at"))
                                    0)
        :enqueued ,(reduce (lambda (count queue)
                             (+ count
                                 (red:llen (redis-key "queue" queue))))
                           queues
                           :initial-value 0)))))
