(in-package :cl-user)
(defpackage redqing-test.worker.processor
  (:use #:cl
        #:prove
        #:redqing.worker.processor)
  (:shadowing-import-from #:redqing.worker.processor
                          #:run
                          #:decode-job
                          #:processor-thread)
  (:import-from #:redqing.connection
                #:connect
                #:disconnect
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.client
                #:enqueue)
  (:import-from #:redqing.util.redis
                #:redis-key))
(in-package :redqing-test.worker.processor)

(plan nil)

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
           (with-redis-connection conn
             (red:del (redis-key "queue" "test")))
           ;; Enqueue a job
           (enqueue conn 'deferred-job nil "test"))
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
          (make-processor :queues '("test"))))
    (diag "start")
    (start processor :timeout 1)
    (is (processor-stopped-p processor) nil)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (diag "stop")
    (stop processor)
    (sleep 2)
    (is (processor-stopped-p processor) t)
    (is (processor-thread processor) nil)

    (diag "kill")
    (start processor :timeout 5)
    (is (processor-stopped-p processor) nil)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (kill processor)
    (sleep 1)
    (is (processor-stopped-p processor) t)
    (is (processor-thread processor) nil)))

(subtest "perform"
  (let ((conn (connect)))
    (unwind-protect
         (progn
           ;; Clear
           (with-redis-connection conn
             (red:del (redis-key "queue" "test")))
           ;; Enqueue a job
           (enqueue conn 'deferred-job nil "test"))
      (disconnect conn)))
  (setf *perform-result* nil)
  (let ((processor
          (make-processor :queues '("test"))))
    (start processor)
    (sleep 0.5)
    (is *perform-result* t)
    (kill processor)))

(finalize)
