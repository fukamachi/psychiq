(in-package :cl-user)
(defpackage redqing.client
  (:use #:cl
        #:redqing.specials)
  (:import-from #:redqing.job
                #:encode-job)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:export #:enqueue
           #:enqueue-to))
(in-package :redqing.client)

(defgeneric enqueue (job-class &optional args))

(defmethod enqueue ((job-class symbol) &optional args)
  (enqueue-to *default-queue-name* job-class args))

(defun enqueue-to (queue job-class &optional args)
  (let ((job-info (encode-job (make-instance job-class) args)))
    (enqueue-to-queue queue job-info)))
