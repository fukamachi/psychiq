(in-package :cl-user)
(defpackage psychiq-test.worker
  (:use #:cl
        #:prove
        #:psychiq.worker
        #:psychiq.util))
(in-package :psychiq-test.worker)

(plan 4)

(defclass my-worker (worker) ())

(when (find-method #'perform nil '(my-worker) nil)
  (remove-method #'perform (find-method #'perform nil '(my-worker))))

(is-error (perform (make-instance 'my-worker))
          'simple-error
          "PERFORM is not implemented")

(defmethod perform ((worker my-worker) &rest args)
  (declare (ignore args))
  (format t "~&Hi. I'm a deferred job.~%"))

(is-print (perform (make-instance 'my-worker))
          "Hi. I'm a deferred job.
"
          "Job's perform method")

(subtest "No args job"
  (let ((job-info (encode-job 'my-worker ())))
    (is-type job-info 'list)
    (is (aget job-info "class") "PSYCHIQ-TEST.WORKER::MY-WORKER"
        "class")
    (is (aget job-info "args") nil
        "args")
    (ok (aget job-info "jid") "jid")
    (ok (aget job-info "created_at") "created_at")))

(subtest "Serializable args"
  (let ((job-info (encode-job 'my-worker (list 1 "2"))))
    (is-type job-info 'list)
    (is (aget job-info "class") "PSYCHIQ-TEST.WORKER::MY-WORKER"
        "class")
    (is (aget job-info "args") '(1 "2")
        "args")
    (ok (aget job-info "jid") "jid")
    (ok (aget job-info "created_at") "created_at")))

(finalize)
