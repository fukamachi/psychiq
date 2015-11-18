(in-package :cl-user)
(defpackage redqing-test.worker.processor
  (:use #:cl
        #:prove
        #:redqing.worker.processor)
  (:shadowing-import-from #:redqing.worker.processor
                          #:run
                          #:finalize
                          #:decode-job
                          #:processor-thread)
  (:import-from #:redqing.connection
                #:connect
                #:disconnect
                #:with-connection)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.client
                #:enqueue-to)
  (:import-from #:redqing.util.redis
                #:redis-key))
(in-package :redqing-test.worker.processor)

(plan 5)

(subtest "processor"
  (let ((processor (make-processor :queues '("test"))))
    (is-type processor 'processor
             "Can make a PROCESSOR")
    (ok (processor-stopped-p processor)
        "PROCESSOR is stopped at first")
    (is (princ-to-string processor)
        "#<PROCESSOR QUEUES: (test) / STATUS: STOPPED>")))

(defparameter *perform-result* nil)

(defclass deferred-job (job) ())
(defmethod perform ((job deferred-job) &rest args)
  (declare (ignore args))
  (setf *perform-result* t))

(subtest "fetch-job & decode-job"
  (let ((conn (connect)))
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "test"))
             ;; Enqueue a job
             (enqueue-to "test" 'deferred-job)))
      (disconnect conn)))
  ;; Fetch a job
  (let* ((processor
           (make-processor :queues '("test")))
         (job-info (fetch-job processor)))
    (ok job-info "Can fetch-job")
    (is-type (decode-job job-info) 'deferred-job
             "Can decode-job")))

(subtest "start, stop & kill"
  (let ((processor
          (make-processor :queues '("test") :timeout 1)))
    (diag "start")
    (start processor)
    (is (processor-stopped-p processor) nil)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (diag "stop")
    (stop processor)
    (sleep 2)
    (is (processor-stopped-p processor) t)
    (is (processor-thread processor) nil)

    (diag "kill")
    (start processor)
    (is (processor-stopped-p processor) nil)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (kill processor)
    (is (processor-stopped-p processor) t)))

(subtest "perform"
  (let ((conn (connect)))
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "test"))
             ;; Enqueue a job
             (enqueue-to "test" 'deferred-job)))
      (disconnect conn)))
  (setf *perform-result* nil)
  (let ((processor
          (make-processor :queues '("test"))))
    (start processor)
    (sleep 0.5)
    (is *perform-result* t)
    (kill processor)))

(is (remove-if-not (lambda (thread)
                     (alexandria:starts-with-subseq "redqing " (bt:thread-name thread)))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(prove:finalize)
