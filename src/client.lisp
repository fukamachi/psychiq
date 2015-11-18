(in-package :cl-user)
(defpackage redqing.client
  (:use #:cl
        #:redqing.specials)
  (:import-from #:redqing.job
                #:encode-job)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:export #:enqueue))
(in-package :redqing.client)

(defgeneric enqueue (job-class &optional args queue))

(defmethod enqueue ((job-class symbol) &optional args (queue *default-queue-name*))
  (let ((job-info (encode-job (make-instance job-class) args)))
    (enqueue-to-queue queue
                      job-info)))
