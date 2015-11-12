(in-package :cl-user)
(defpackage redqing.job
  (:use #:cl)
  (:export #:job
           #:perform
           #:fail-job))
(in-package :redqing.job)

(defclass job () ())

(defgeneric perform (job &rest args)
  (:method ((job job) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of job))))
  (:method :around ((job job) &rest args)
    (declare (ignore args))
    (handler-bind ((error
                     (lambda (exception)
                       (fail-job job exception))))
      (call-next-method))))

(defgeneric fail-job (job exception)
  (:method ((job job) exception)
    (vom:info "~A failed: ~A" job exception)
    ;; TODO sava-failure
    ))
