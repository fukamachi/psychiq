(in-package :cl-user)
(defpackage redqing.job
  (:use #:cl)
  (:import-from #:redqing.util
                #:symbol-name-with-package)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf)
  (:export #:job
           #:job-id
           #:perform
           #:encode-job
           #:decode-job))
(in-package :redqing.job)

(defun generate-random-id (&optional (length 12))
  (format nil "~(~36,8,'0R~)" (random (expt 36 length))))

(defclass job ()
  ((id :initarg :id
       :initform (generate-random-id)
       :accessor job-id)))

(defgeneric perform (job &rest args)
  (:method ((job job) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of job)))))

(defun encode-job (job-class args)
  `(("class" . ,(symbol-name-with-package job-class))
    ("args" . ,(prin1-to-string (marshal:marshal args)))
    ("jid" . ,(generate-random-id))
    ("created_at" . ,(timestamp-to-unix (now)))))

(defun decode-job (job-info)
  (let ((class (assoc "class" job-info :test #'string=))
        (args  (assoc "args"  job-info :test #'string=))
        (jid   (assoc "jid"   job-info :test #'string=)))
    (unless (and class args jid)
      (error "Invalid job: ~S" job-info))
    (let ((class (read-from-string (cdr class))))
      (check-type class symbol)
      (let ((job (make-instance class
                                :id (cdr jid))))
        (check-type job job)
        (values job
                (marshal:unmarshal (read-from-string (cdr args))))))))
