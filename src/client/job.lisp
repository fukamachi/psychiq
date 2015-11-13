(in-package :cl-user)
(defpackage redqing.job
  (:use #:cl)
  (:export #:job
           #:perform))
(in-package :redqing.job)

(defclass job () ())

(defgeneric perform (job &rest args)
  (:method ((job job) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of job)))))
