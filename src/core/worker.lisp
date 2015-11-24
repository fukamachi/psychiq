(in-package :cl-user)
(defpackage psychiq.worker
  (:use #:cl
        #:psychiq.specials)
  (:import-from #:psychiq.util
                #:symbol-name-with-package)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf)
  (:export #:worker
           #:perform
           #:max-retries
           #:encode-job
           #:decode-job))
(in-package :psychiq.worker)

(defun generate-random-id (&optional (length 12))
  (format nil "~(~36,8,'0R~)" (random (expt 36 length))))

(defclass worker () ())

(defgeneric perform (worker &rest args)
  (:method ((worker worker) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of worker)))))

(defgeneric max-retries (worker)
  (:method ((worker worker))
    *default-max-retry-attempts*))

(defun encode-job (worker-class args)
  `(("class" . ,(symbol-name-with-package worker-class))
    ("args" . ,(mapcar (lambda (arg)
                         (prin1-to-string (marshal:marshal arg)))
                       args))
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
      (let ((worker (make-instance class)))
        (check-type worker worker)
        (values worker
                (mapcar (lambda (arg)
                          (marshal:unmarshal (read-from-string arg)))
                        (cdr args)))))))
