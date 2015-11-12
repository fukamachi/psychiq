(in-package :cl-user)
(defpackage redqing
  (:nicknames #:redq)
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:connection-coder)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:import-from #:redqing.coder
                #:encode)
  (:export #:enqueue))
(in-package :redqing)

(cl-reexport:reexport-from :redqing.connection
                           :include '(#:connect #:disconnect #:reconnect #:with-redis-connection #:connectedp))

(cl-reexport:reexport-from :redqing.job
                           :include '(#:job #:perform #:fail-job))

(defgeneric enqueue (connection queue job-class &optional args))

(defmethod enqueue ((conn connection) queue (job-class symbol) &optional args)
  (let ((payload (encode (connection-coder conn)
                         `(("class" . ,job-class)
                           ("args" . ,args)))))
    (enqueue-to-queue conn
                      queue
                      payload)))
