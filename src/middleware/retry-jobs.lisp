(in-package :cl-user)
(defpackage psychiq.middleware.retry-jobs
  (:use #:cl
        #:psychiq.specials
        #:psychiq.util)
  (:import-from #:psychiq.connection
                #:with-connection)
  (:import-from #:psychiq.worker
                #:max-retries)
  (:import-from #:psychiq.coder
                #:encode-object)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf)
  (:export #:*psychiq-middleware-retry-jobs*))
(in-package :psychiq.middleware.retry-jobs)

(defparameter *psychiq-middleware-retry-jobs*
  (lambda (next)
    (lambda (conn job-info queue)
      (block middleware
        (handler-bind ((error
                         (lambda (e)
                           (let ((worker-class
                                   (ignore-errors
                                    (read-from-string (aget job-info "class")))))
                             (when worker-class
                               (attempt-retry conn queue worker-class job-info e)
                               (return-from middleware nil))))))
          (funcall next job-info))))))

(defun backtrace (&optional (count-to-remove 0))
  ;; Remove the first stack (this function) anyway.
  (incf count-to-remove)
  (with-output-to-string (*standard-output*)
    (map nil
         (lambda (stack)
           (let ((*print-pretty* nil)
                 (*print-readably* nil))
             (format t "~&~D: ~:[~S ~S~;(~S~{ ~S~})~]~%"
                     (- (dissect:pos stack) count-to-remove)
                     (listp (dissect:args stack))
                     (dissect:call stack)
                     (dissect:args stack))))
         (nthcdr count-to-remove (dissect:stack)))))

(defun attempt-retry (conn queue worker-class job-info e)
  (let ((options '())
        (max-retries
          (max-retries (allocate-instance (find-class worker-class)))))
    (setf max-retries
          (if (numberp max-retries)
              max-retries
              *default-max-retry-attempts*))

    (nconcf options
            `(("error_message" . ,(princ-to-string e))
              ("error_class" . ,(symbol-name-with-package (class-name (class-of e))))
              ("error_backtrace" . ,(backtrace 1))))

    (setf (aget options "queue") queue)

    (let ((retry-count (aget job-info "retry_count")))
      (if retry-count
          (setf (aget options "retry_count") (1+ retry-count)
                (aget options "retried_at") (timestamp-to-unix (now)))
          (progn
            (setf retry-count 0)
            (nconcf options
                    `(("failed_at" . ,(timestamp-to-unix (now)))
                      ("retry_count" . 0)))))

      (loop for (option . value) in options
            do (setf (aget job-info option) value))

      (cond
        ((< retry-count max-retries)
         (let* ((delay (delay-for retry-count))
                (retry-at (+ (timestamp-to-unix (now)) delay)))
           (vom:info "Failure! Retry ~A in ~A seconds"
                     retry-count
                     delay)
           (let ((payload (encode-object job-info)))
             (with-connection conn
               (red:zadd (redis-key "retry")
                         (princ-to-string retry-at)
                         payload)))))
        (t
         (send-to-morgue conn job-info))))))

(defun delay-for (retry-count)
  (+ (expt retry-count 4) 15 (* (random 30) (1+ retry-count))))

(defparameter *dead-timeout-in-seconds*
  ;; 6 months
  (* (* 24 60 60) 180))

(defparameter *dead-max-jobs*
  10000)

(defun send-to-morgue (conn job-info)
  (vom:info "Adding dead ~S job ~S"
            (aget job-info "class")
            (aget job-info "jid"))
  (let ((payload (encode-object job-info))
        (now (timestamp-to-unix (now))))
    (with-connection conn
      (with-redis-transaction
        (red:zadd (redis-key "dead")
                  now
                  payload)
        (red:zremrangebyscore (redis-key "dead")
                              "-inf"
                              (- now *dead-timeout-in-seconds*))
        (red:zremrangebyrank (redis-key "dead")
                             0
                             -10000)))))
