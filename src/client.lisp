(in-package :cl-user)
(defpackage redqing.client
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection)
  (:import-from #:redqing.job
                #:encode-job)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:export #:enqueue))
(in-package :redqing.client)

(defgeneric enqueue (connection job-class &optional args queue))

(defmethod enqueue ((conn connection) (job-class symbol) &optional args (queue "default"))
  (let ((job-info (encode-job (make-instance job-class) args)))
    (enqueue-to-queue conn
                      queue
                      job-info)))
