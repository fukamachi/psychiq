(in-package :cl-user)
(defpackage redqing-test.worker
  (:use #:cl
        #:prove
        #:redqing.worker)
  (:shadowing-import-from #:redqing.worker
                          #:run)
  (:import-from #:redqing.worker
                #:make-worker
                #:worker))
(in-package :redqing-test.worker)

(plan 2)

(subtest "make-worker"
  (let ((worker (make-worker)))
    (is-type worker 'worker)
    (is (princ-to-string worker)
        "#<WORKER REDIS: localhost:6379 / PROCESSORS: 25 / QUEUE: (default) / STATUS: STOPPED>")))

(subtest "start, stop & kill"
  (let ((worker (make-worker)))
    (diag "start")
    (is (worker-status worker) :stopped)
    (is (start worker :timeout 1) worker)
    (is (worker-status worker) :running)
    (sleep 1)

    (diag "stop")
    (ok (stop worker))
    (is (worker-status worker) :stopped)

    (diag "kill")
    (start worker :timeout 1)
    (is (worker-status worker) :running)
    (kill worker)
    (is (worker-status worker) :stopped)))

(finalize)
