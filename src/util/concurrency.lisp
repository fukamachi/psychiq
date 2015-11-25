(in-package :cl-user)
(defpackage psychiq.util.concurrency
  (:use #:cl)
  (:export #:make-thread-safe-variable
           #:get-value
           #:reset-value))
(in-package :psychiq.util.concurrency)

(defstruct (thread-safe-variable
            (:constructor make-thread-safe-variable
                (initial-value &key lock &aux (value initial-value))))
  initial-value
  value
  (lock (bt:make-recursive-lock)))

(defun get-value (var)
  (thread-safe-variable-value var))

(defun (setf get-value) (new-value var)
  (bt:with-recursive-lock-held ((thread-safe-variable-lock var))
    (setf (thread-safe-variable-value var) new-value)))

(defun reset-value (var)
  (bt:with-recursive-lock-held ((thread-safe-variable-lock var))
    (prog1 (thread-safe-variable-value var)
      (setf (thread-safe-variable-value var)
            (thread-safe-variable-initial-value var)))))
