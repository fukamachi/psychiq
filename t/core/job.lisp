(in-package :cl-user)
(defpackage redqing-test.job
  (:use #:cl
        #:prove
        #:redqing.job))
(in-package :redqing-test.job)

(plan 2)

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

(finalize)
