(in-package :cl-user)
(defpackage psychiq-test.launcher
  (:use #:cl
        #:prove
        #:psychiq.launcher)
  (:shadowing-import-from #:psychiq.launcher
                          #:run)
  (:import-from #:psychiq.launcher
                #:make-launcher
                #:launcher))
(in-package :psychiq-test.launcher)

(plan 5)

(subtest "make-launcher"
  (let ((launcher (make-launcher)))
    (is-type launcher 'launcher)
    (is (princ-to-string launcher)
        "#<LAUNCHER REDIS: localhost:6379 / CONCURRENCY: 25 / QUEUE: (default) / STATUS: STOPPED>")))

(subtest "start, stop & kill"
  (let ((launcher (make-launcher :interval 1)))
    (diag "start")
    (is (launcher-status launcher) :stopped)
    (is (start launcher) launcher)
    (is (launcher-status launcher) :running)
    (sleep 1)

    (diag "stop")
    (ok (stop launcher))
    (is (launcher-status launcher) :stopping)
    (sleep 1.2)

    (diag "kill")
    (start launcher)
    (is (launcher-status launcher) :running)
    (kill launcher)
    (ok (find (launcher-status launcher) '(:stopping :stopped)))))

(sleep 3)
(is (remove-if-not (lambda (thread)
                     (and (bt:thread-alive-p thread)
                          (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(subtest "can kill long interval launcher"
  (let ((launcher (make-launcher :interval 120))
        (threads (bt:all-threads)))
    (start launcher)
    (sleep 1)
    (kill launcher)

    (sleep 3)

    (is (remove-if-not (lambda (thread)
                         (and (bt:thread-alive-p thread)
                              (not (find thread threads :test #'eq))
                              (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                       (bt:all-threads))
        nil
        "All launcher threads has been terminated")))

(is (remove-if-not (lambda (thread)
                     (and (bt:thread-alive-p thread)
                          (alexandria:starts-with-subseq "psychiq " (bt:thread-name thread))))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(finalize)
