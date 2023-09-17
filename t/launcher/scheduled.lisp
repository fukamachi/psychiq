(in-package :cl-user)
(defpackage psychiq-test.launcher.scheduled
  (:use #:cl
        #:prove
        #:psychiq.launcher.scheduled)
  (:import-from #:psychiq.launcher.scheduled
                #:scheduled-thread
                #:enqueue-jobs)
  (:import-from #:psychiq.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:psychiq.util.redis
                #:redis-key))
(in-package :psychiq-test.launcher.scheduled)

(defvar *threads* (bt2:all-threads))

(plan 4)

(subtest "make-scheduled"
  (let ((scheduled (make-scheduled)))
    (is-type scheduled 'scheduled)
    (is (scheduled-status scheduled) :stopped)
    (is (scheduled-thread scheduled) nil)))

(subtest "start, stop & kill"
  (let ((scheduled (make-scheduled)))
    (diag "start")
    (is (start scheduled) scheduled)
    (ok (bt2:threadp (scheduled-thread scheduled)))
    (is (scheduled-status scheduled) :running)

    (diag "stop")
    (stop scheduled)
    (ok (find (scheduled-status scheduled) '(:stopping :stopped)))
    (sleep 3)
    (is (scheduled-status scheduled) :stopped)
    (is (scheduled-thread scheduled) nil)

    (diag "kill")
    (start scheduled)
    (kill scheduled)
    (ok (find (scheduled-status scheduled) '(:stopping :stopped)))))

(defclass my-worker (psy:worker) ())
(defmethod psy:perform ((worker my-worker) &rest args)
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
                     "{\"class\":\"PSYCHIQ-TEST.LAUNCHER.SCHEDULED::MY-WORKER\",\"args\":[],\"jid\":\"b1ly5y10yia9\",\"enqueued_at\":1447827023,\"created_at\":1447827023,\"error_message\":\"Failed\",\"error_class\":\"COMMON-LISP::SIMPLE-ERROR\",\"failed_at\":1447827023,\"retry_count\":0,\"queue\":\"test\"}")
           (enqueue-jobs (1+ now))
           (is (red:zrange (redis-key "retry") 0 1) nil)
           (let ((jobs (red:lrange (redis-key "queue" "test") 0 -1)))
             (is (length jobs) 1)
             (like (first jobs) "\"jid\":\"b1ly5y10yia9\"")))
      (disconnect conn))))

(is (bt2:all-threads) *threads*
    "All threads has been terminated")

(finalize)
