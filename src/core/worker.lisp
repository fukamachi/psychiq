(in-package :cl-user)
(defpackage psychiq.worker
  (:use #:cl
        #:psychiq.util
        #:psychiq.specials)
  (:import-from #:psychiq.util
                #:symbol-name-with-package)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf
                #:if-let
                #:with-gensyms)
  (:export #:worker
           #:perform
           #:worker-max-retries
           #:worker-use-dead-queue-p
           #:worker-queue-name
           #:worker-use-backtrace-p
           #:encode-job
           #:decode-job))
(in-package :psychiq.worker)

(defclass worker () ())

(defgeneric perform (worker &rest args)
  (:method ((worker worker) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of worker)))))

(defgeneric worker-max-retries (worker)
  (:method ((worker worker))
    *default-max-retry-attempts*))
(defgeneric worker-use-dead-queue-p (worker)
  (:method ((worker worker))
    t))
(defgeneric worker-queue-name (worker)
  (:method ((worker worker))
    *default-queue-name*))
(defgeneric worker-use-backtrace-p (worker)
  (:method ((worker worker))
    nil))

(defun encode-job (worker-class args)
  `(("class" . ,(symbol-name-with-package worker-class))
    ("args" . ,args)
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
        worker))))
