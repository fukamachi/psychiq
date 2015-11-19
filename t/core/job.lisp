(in-package :cl-user)
(defpackage redqing-test.job
  (:use #:cl
        #:prove
        #:redqing.job
        #:redqing.util))
(in-package :redqing-test.job)

(plan 5)

(defclass deferred-job (job) ())

(when (find-method #'perform nil '(deferred-job) nil)
  (remove-method #'perform (find-method #'perform nil '(deferred-job))))

(is-error (perform (make-instance 'deferred-job))
          'simple-error
          "PERFORM is not implemented")

(defmethod perform ((job deferred-job) &rest args)
  (declare (ignore args))
  (format t "~&Hi. I'm a deferred job.~%"))

(is-print (perform (make-instance 'deferred-job))
          "Hi. I'm a deferred job.
"
          "Job's perform method")

(subtest "No args job"
  (let ((job-info (encode-job 'deferred-job ())))
    (is-type job-info 'list)
    (is (aget job-info "class") "REDQING-TEST.JOB::DEFERRED-JOB"
        "class")
    (is (aget job-info "args") "(:PCODE 1 (:LIST 1))"
        "args")
    (ok (aget job-info "jid") "jid")
    (ok (aget job-info "created_at") "created_at")))

(subtest "Serializable args"
  (let ((job-info (encode-job 'deferred-job (list 1 "2"))))
    (is-type job-info 'list)
    (is (aget job-info "class") "REDQING-TEST.JOB::DEFERRED-JOB"
        "class")
    (is (aget job-info "args") "(:PCODE 1 (:LIST 1 1 (:SIMPLE-STRING 2 \"2\")))"
        "args")
    (ok (aget job-info "jid") "jid")
    (ok (aget job-info "created_at") "created_at")))

(subtest "Encoding job object"
  (let* ((job (make-instance 'deferred-job))
         (job-info (encode-job job ())))
    (is-type job-info 'list)
    (is (aget job-info "class") "REDQING-TEST.JOB::DEFERRED-JOB"
        "class")
    (is (aget job-info "args") "(:PCODE 1 (:LIST 1))"
        "args")
    (ok (aget job-info "jid") "jid")
    (ok (aget job-info "created_at") "created_at")))

(finalize)
