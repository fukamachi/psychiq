(in-package :cl-user)
(defpackage redqing.middleware.retry-jobs
  (:use #:cl)
  (:import-from #:redqing.connection
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:job-options
                #:encode-job)
  (:import-from #:redqing.coder
                #:encode-object)
  (:import-from #:redqing.redis
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
  (declare (ignore queue))
  (let* ((options (job-options job))
         (max-retries (aget options "max_retries")))
    (setf max-retries
          (if (numberp max-retries)
              max-retries
              *default-max-retry-attempts*))

    (nconcf options
            `(("error_message" . ,(princ-to-string e))
              ("error_class" . ,(symbol-name-with-package (class-name (class-of e))))))

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
         ;; TODO: give up a job
         )))))

(defun delay-for (retry-count)
  (+ (expt retry-count 4) 15 (* (random 30) (1+ retry-count))))
