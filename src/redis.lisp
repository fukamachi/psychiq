(in-package :cl-user)
(defpackage redqing.redis
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:with-transaction))
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
