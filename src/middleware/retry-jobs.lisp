(in-package :cl-user)
(defpackage redqing.middleware.retry-jobs
  (:use #:cl)
  (:import-from #:redqing.connection
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:job-id
                #:job-options
                #:encode-job)
  (:import-from #:redqing.coder
                #:encode-object)
  (:import-from #:redqing.redis
                #:with-transaction
                #:redis-key)
  (:import-from #:redqing.util
                #:symbol-name-with-package)
  (:import-from #:redqing.util.assoc
                #:aget)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf)
  (:export #:*redqing-middleware-retry-jobs*))
(in-package :redqing.middleware.retry-jobs)

(defparameter *default-max-retry-attempts* 25)

(defparameter *redqing-middleware-retry-jobs*
  (lambda (next)
    (lambda (conn job queue args)
      (handler-bind ((error
                       (lambda (e)
                         (attempt-retry conn queue job args e))))
        (funcall next job args)))))

(defun attempt-retry (conn queue job args e)
  (let* ((options (job-options job))
         (max-retries (aget options "max_retries")))
    (setf max-retries
          (if (numberp max-retries)
              max-retries
              *default-max-retry-attempts*))

    (nconcf options
            `(("error_message" . ,(princ-to-string e))
              ("error_class" . ,(symbol-name-with-package (class-name (class-of e))))))

    (setf (aget options "queue") queue)

    (let ((retry-count (aget options "retry_count")))
      (if retry-count
          (setf (aget options "retry_count") (1+ retry-count)
                (aget options "retried_at") (timestamp-to-unix (now)))
          (progn
            (setf retry-count 0)
            (nconcf options
                    `(("failed_at" . ,(timestamp-to-unix (now)))
                      ("retry_count" . 0)))))

      (setf (job-options job) options)

      (cond
        ((< retry-count max-retries)
         (let* ((delay (delay-for retry-count))
                (retry-at (+ (timestamp-to-unix (now)) delay)))
           (vom:info "Failure! Retry ~A in ~A seconds"
                     retry-count
                     delay)
           (let ((payload
                   (encode-object (encode-job job args))))
             (with-redis-connection conn
               (red:zadd (redis-key "retry")
                         (princ-to-string retry-at)
                         payload)))))
        (t
         (send-to-morgue job args))))))

(defun delay-for (retry-count)
  (+ (expt retry-count 4) 15 (* (random 30) (1+ retry-count))))

(defparameter *dead-timeout-in-seconds*
  ;; 6 months
  (* (* 24 60 60) 180))

(defparameter *dead-max-jobs*
  10000)

(defun send-to-morgue (job args)
  (vom:info "Adding dead ~S job ~S"
            (class-name (class-of job))
            (job-id job))
  (let ((payload (encode-object (encode-job job args)))
        (now (timestamp-to-unix (now))))
    (with-transaction
      (red:zadd (redis-key "dead")
                now
                payload)
      (red:zremrangebyscore (redis-key "dead")
                            "-inf"
                            (- now *dead-timeout-in-seconds*))
      (red:zremrangebyrank (redis-key "dead")
                           0
                           -10000))))
