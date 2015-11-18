(in-package :cl-user)
(defpackage redqing-test.worker.scheduled
  (:use #:cl
        #:prove
        #:redqing.worker.scheduled)
  (:import-from #:redqing.worker.scheduled
                #:scheduled-thread
                #:enqueue-jobs)
  (:import-from #:redqing.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:redqing.util.redis
                #:redis-key))
(in-package :redqing-test.worker.scheduled)

(defvar *threads* (bt:all-threads))

(plan 4)

(subtest "make-scheduled"
  (let ((scheduled (make-scheduled)))
    (is-type scheduled 'scheduled)
    (ok (scheduled-stopped-p scheduled))
    (is (scheduled-thread scheduled) nil)))

(subtest "start, stop & kill"
  (let ((scheduled (make-scheduled)))
    (diag "start")
    (is (start scheduled) scheduled)
    (ok (bt:threadp (scheduled-thread scheduled)))
    (is (scheduled-stopped-p scheduled) nil)

    (diag "stop")
    (stop scheduled)
    (is (scheduled-stopped-p scheduled) t)
    (sleep 3)
    (is (scheduled-thread scheduled) nil)

    (diag "kill")
    (start scheduled)
    (kill scheduled)
    (is (scheduled-stopped-p scheduled) t)))

(defclass deferred-job (redq:job) ())
(defmethod redq:perform ((job deferred-job) &rest args)
  (declare (ignore args))
  "OK")

(subtest "enqueue-jobs"
  (let ((conn (connect))
        (now (local-time:timestamp-to-unix (local-time:now))))
    (unwind-protect
         (with-connection conn
           (red:del (redis-key "retry"))
           (red:del (redis-key "queue" "test"))
           (red:zadd (redis-key "retry")
                     now
                     "{\"class\":\"REDQING-TEST.WORKER.SCHEDULED::DEFERRED-JOB\",\"args\":[],\"jid\":\"b1ly5y10yia9\",\"enqueued_at\":1447827023,\"created_at\":1447827023,\"error_message\":\"Failed\",\"error_class\":\"COMMON-LISP::SIMPLE-ERROR\",\"failed_at\":1447827023,\"retry_count\":0,\"queue\":\"test\"}")
           (enqueue-jobs (1+ now))
           (is (red:zrange (redis-key "retry") 0 1) nil)
           (let ((jobs (red:lrange (redis-key "queue" "test") 0 -1)))
             (is (length jobs) 1)
             (like (first jobs) "\"jid\":\"b1ly5y10yia9\"")))
      (disconnect conn))))

(is (bt:all-threads) *threads*
    "All threads has been terminated")

(finalize)
