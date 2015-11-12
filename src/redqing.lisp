(in-package :cl-user)
(defpackage redqing
  (:nicknames #:redq)
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection)
  (:import-from #:redqing.job
                #:job
                #:enqueue-job)
  (:export #:enqueue))
(in-package :redqing)

(cl-reexport:reexport-from :redqing.connection
                           :include '(#:connect #:disconnect #:reconnect #:with-redis-connection #:connectedp))

(cl-reexport:reexport-from :redqing.job
                           :include '(#:job #:job-class #:job-args #:job-queue #:perform #:enqueue-job #:fail-job))

(defgeneric enqueue (connection queue job-or-job-class &optional args))

(defmethod enqueue ((connection connection) queue (job job) &optional args)
  (declare (ignore queue args))
  (enqueue-job connection job))

(defmethod enqueue ((connection connection) queue (job-class symbol) &optional args)
  (enqueue connection queue
           (make-instance job-class
                          :args args
                          :queue queue)))
