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

(defun symbol-name-with-package (symbol)
  (let ((package (symbol-package symbol)))
    (unless package
      (error "Uninterned symbol is not allowed"))
    (format nil "~A::~A"
            (package-name package)
            (symbol-name symbol))))

(defgeneric enqueue (connection job-class &optional args queue))

(defmethod enqueue ((conn connection) (job-class symbol) &optional args (queue "default"))
  (let ((payload `(("class" . ,(symbol-name-with-package job-class))
                   ("args" . ,args)
                   ("jid" . ,(generate-random-id))
                   ("created_at" . ,(timestamp-to-unix (now))))))
    (enqueue-to-queue conn
                      queue
                      payload)))
