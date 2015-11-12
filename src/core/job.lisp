(in-package :cl-user)
(defpackage redqing.job
  (:use #:cl)
  (:export #:job
           #:perform
           #:fail-job))
(in-package :redqing.job)

(defclass job () ())

(defgeneric perform (job)
  (:method ((job job))
    ;; nothing to do
    )
  (:method :around ((job job))
    (handler-bind ((error
                     (lambda (exception)
                       (fail-job job exception))))
      (call-next-method))))

(defgeneric fail-job (job exception)
  (:method ((job job) exception)
    (vom:info "~A failed: ~A" job exception)
    ;; TODO sava-failure
    ))
