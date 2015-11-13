(in-package :cl-user)
(defpackage redqing-test.worker.processor
  (:use #:cl
        #:prove
        #:redqing.worker.processor)
  (:shadowing-import-from #:redqing.worker.processor
                          #:run
                          #:decode-job)
  (:import-from #:redqing.connection
                #:connect
                #:disconnect
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.redis
                #:redis-key)
  (:import-from #:redqing.client
                #:enqueue))
(in-package :redqing-test.worker.processor)

(plan nil)

(subtest "processor"
  (let ((conn (connect :host "localhost" :port 6379)))
    (unwind-protect
         (let ((processor
                 (make-processor :connection conn
                                 :queues '())))
           (is-type processor 'processor
                    "Can make a PROCESSOR")
           (ok (processor-stopped-p processor)
               "PROCESSOR is stopped at first"))
      (disconnect conn))))

(defclass deferred-job (job) ())
(defmethod perform ((job deferred-job) &rest args)
  (declare (ignore args)))

(subtest "fetch-job & decode-job"
  (let ((conn (connect :host "localhost" :port 6379)))
    (unwind-protect
         (progn
           ;; Clear
           (with-redis-connection conn
             (red:del (redis-key "queue" "test")))
           ;; Enqueue a job
           (enqueue conn "test" 'deferred-job)
           ;; Fetch a job
           (let* ((processor
                    (make-processor :connection conn
                                    :queues '("test")))
                  (job-info (fetch-job processor)))
             (ok job-info "Can fetch-job")
             (is-type (decode-job job-info) 'deferred-job
                      "Can decode-job")))
      (disconnect conn))))

(finalize)
