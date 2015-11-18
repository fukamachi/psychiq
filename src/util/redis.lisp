(in-package :cl-user)
(defpackage redqing.util.redis
  (:use #:cl)
  (:import-from #:redqing.specials
                #:*redqing-namespace*)
  (:import-from #:alexandria
                #:with-gensyms
                #:starts-with-subseq)
  (:export #:with-redis-transaction

           #:*redqing-namespace*
           #:redis-key
           #:omit-redis-prefix))
(in-package :redqing.util.redis)

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
  (format nil "~A:~{~A~^:~}"
          *redqing-namespace*
          keys))

(defun omit-redis-prefix (key &rest prefixes)
  (let ((prefix (format nil (apply #'redis-key prefixes) ":")))
    (unless (starts-with-subseq prefix key)
      (error "~S does not start with ~S" key prefix))
    (subseq key (length prefix))))
