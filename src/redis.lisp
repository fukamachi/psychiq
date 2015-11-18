(in-package :cl-user)
(defpackage redqing.redis
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:starts-with-subseq)
  (:export #:with-transaction

           #:*redqing-namespace*
           #:redis-key
           #:omit-redis-prefix))
(in-package :redqing.redis)

(defmacro with-transaction (&body body)
  (with-gensyms (ok)
    `(let (,ok)
       (red:multi)
       (unwind-protect (multiple-value-prog1
                           (progn ,@body)
                         (setf ,ok t))
         (if ,ok
             (red:exec)
             (red:discard))))))

(defvar *redqing-namespace* "redqing")

(defun redis-key (&rest keys)
  (format nil "~A:~{~A~^:~}"
          *redqing-namespace*
          keys))

(defun omit-redis-prefix (key &rest prefixes)
  (let ((prefix (format nil (apply #'redis-key prefixes) ":")))
    (unless (starts-with-subseq prefix key)
      (error "~S does not start with ~S" key prefix))
    (subseq key (length prefix))))
