(in-package :cl-user)
(defpackage psychiq-test.worker
  (:use #:cl
        #:prove
        #:psychiq.worker)
  (:shadowing-import-from #:psychiq.worker
                          #:run)
  (:import-from #:psychiq.worker
                #:make-worker
                #:worker))
(in-package :psychiq-test.worker)

(plan 5)

(subtest "make-worker"
  (let ((worker (make-worker)))
    (is-type worker 'worker)
    (is (princ-to-string worker)
        "#<WORKER REDIS: localhost:6379 / CONCURRENCY: 25 / QUEUE: (default) / STATUS: STOPPED>")))

(subtest "start, stop & kill"
  (let ((worker (make-worker :interval 1)))
    (diag "start")
    (is (worker-status worker) :stopped)
    (is (start worker) worker)
    (is (worker-status worker) :running)
    (sleep 1)

    (diag "stop")
    (ok (stop worker))
    (is (worker-status worker) :stopping)
    (sleep 1.2)

    (diag "kill")
    (start worker)
    (is (worker-status worker) :running)
    (kill worker)
    (ok (find (worker-status worker) '(:stopping :stopped)))))

(sleep 3)
(is (remove-if-not (lambda (thread)
                     (and (bt:thread-alive-p thread)
                          (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(subtest "can kill long interval worker"
  (let ((worker (make-worker :interval 120))
        (threads (bt:all-threads)))
    (start worker)
    (sleep 1)
    (kill worker)

    (sleep 3)

    (is (remove-if-not (lambda (thread)
                         (and (bt:thread-alive-p thread)
                              (not (find thread threads :test #'eq))
                              (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                       (bt:all-threads))
        nil
        "All worker threads has been terminated")))

(is (remove-if-not (lambda (thread)
                     (and (bt:thread-alive-p thread)
                          (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(finalize)
