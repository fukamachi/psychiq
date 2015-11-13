(in-package :cl-user)
(defpackage redqing.client
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:enqueue))
(in-package :redqing.client)

(defun generate-random-id (&optional (length 12))
  (format nil "~(~36,8,'0R~)" (random (expt 36 length))))

(defgeneric enqueue (connection queue job-class &optional args))

(defmethod enqueue ((conn connection) queue (job-class symbol) &optional args)
  (let ((payload `(("class" . ,job-class)
                   ("args" . ,args)
                   ("jid" . ,(generate-random-id))
                   ("created_at" . ,(timestamp-to-unix (now))))))
    (enqueue-to-queue conn
                      queue
                      payload)))
