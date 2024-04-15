(in-package :cl-user)
(defpackage psychiq.util.redis
  (:use #:cl)
  (:import-from #:psychiq.specials
                #:*psychiq-namespace*)
  (:import-from #:alexandria
                #:with-gensyms
                #:starts-with-subseq)
  (:export #:with-redis-transaction

           #:*psychiq-namespace*
           #:redis-key
           #:omit-redis-prefix))
(in-package :psychiq.util.redis)

(defmacro with-redis-transaction (&body body)
  (with-gensyms (ok)
    `(let (,ok)
       (red:multi)
       (unwind-protect (multiple-value-prog1
                           (progn ,@body)
                         (setf ,ok t))
         (if ,ok
             (red:exec)
             (red:discard))))))

(defun redis-key (&rest keys)
  (format nil "~@[~A:~]~{~A~^:~}"
          *psychiq-namespace*
          keys))

(defun omit-redis-prefix (key &rest prefixes)
  (let ((prefix (concatenate 'string (apply #'redis-key prefixes) ":")))
    (unless (starts-with-subseq prefix key)
      (error "~S does not start with ~S" key prefix))
    (subseq key (length prefix))))
