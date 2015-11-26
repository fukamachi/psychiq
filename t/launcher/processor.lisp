(in-package :cl-user)
(defpackage psychiq-test.launcher.processor
  (:use #:cl
        #:prove
        #:psychiq.launcher.processor)
  (:shadowing-import-from #:psychiq.launcher.processor
                          #:run
                          #:finalize
                          #:decode-job
                          #:processor-thread)
  (:import-from #:psychiq.connection
                #:connect
                #:disconnect
                #:with-connection)
  (:import-from #:psychiq.worker
                #:worker
                #:perform
                #:worker-queue-name)
  (:import-from #:psychiq.client
                #:enqueue)
  (:import-from #:psychiq.util.redis
                #:redis-key))
(in-package :psychiq-test.launcher.processor)

(plan 5)

(subtest "processor"
  (let ((processor (make-processor :queues '("test"))))
    (is-type processor 'processor
             "Can make a PROCESSOR")
    (is (processor-status processor)
        :stopped
        "PROCESSOR is stopped at first")
    (is (princ-to-string processor)
        "#<PROCESSOR QUEUES: (test) / STATUS: STOPPED>")))

(defparameter *perform-result* nil)

(defclass my-worker (worker) ())
(defmethod perform ((worker my-worker) &rest args)
  (declare (ignore args))
  (setf *perform-result* t))
(defmethod worker-queue-name ((worker my-worker))
  "test")

(subtest "fetch-job & decode-job"
  (let ((conn (connect)))
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "test"))
             ;; Enqueue a job
             (enqueue 'my-worker)))
      (disconnect conn)))
  ;; Fetch a job
  (let* ((processor
           (make-processor :queues '("test")))
         (job-info (fetch-job processor)))
    (ok job-info "Can fetch-job")
    (is-type (decode-job job-info) 'my-worker
             "Can decode-job")))

(subtest "start, stop & kill"
  (let ((processor
          (make-processor :queues '("test") :timeout 1)))
    (diag "start")
    (start processor)
    (is (processor-status processor) :running)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (diag "stop")
    (stop processor)
    (is (processor-status processor) :stopping)
    (sleep 2)
    (is (processor-status processor) :stopped)
    (is (processor-thread processor) nil)

    (diag "kill")
    (start processor)
    (is (processor-status processor) :running)
    (ok (bt:thread-alive-p (processor-thread processor)))
    (kill processor)
    (ok (find (processor-status processor) '(:stopping :stopped)))))

(subtest "perform"
  (let ((conn (connect)))
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "test"))
             ;; Enqueue a job
             (enqueue 'my-worker)))
      (disconnect conn)))
  (setf *perform-result* nil)
  (let ((processor
          (make-processor :queues '("test"))))
    (start processor)
    (sleep 0.5)
    (is *perform-result* t)
    (kill processor)))

(sleep 3)
(is (remove-if-not (lambda (thread)
                     (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread)))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(prove:finalize)
