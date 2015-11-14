(in-package :cl-user)
(defpackage redqing.worker.processor
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:connection-coder
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.redis
                #:redis-key)
  (:import-from #:redqing.coder
                #:decode)
  (:export #:processor
           #:make-processor
           #:processor-stopped-p
           #:processor-manager
           #:run
           #:start
           #:stop
           #:kill
           #:fetch-job
           #:process-job
           #:perform-job))
(in-package :redqing.worker.processor)

(defstruct processor
  (connection nil :type connection)
  (queues '() :type list)
  (manager nil)
  (thread nil)
  (stopped-p t))

(defgeneric fetch-job (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (let ((ret
            (with-redis-connection (processor-connection processor)
              (apply #'red:blpop
                     (nconc
                      (mapcar (lambda (queue)
                                (redis-key "queue" queue))
                              ;; TODO: allow to shuffle the queues
                              (processor-queues processor))
                      (list timeout))))))
      (if ret
          (destructuring-bind (queue payload) ret
            (vom:debug "Found job on ~A" queue)
            (decode (connection-coder (processor-connection processor))
                    payload))
          nil))))

(defgeneric run (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (loop
      until (processor-stopped-p processor)
      do (let ((job-info (fetch-job processor :timeout timeout)))
           (if job-info
               (process-job processor job-info)
               (vom:debug "Timed out after ~D seconds" timeout))))))

(defgeneric start (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (vom:info "Starting...")
    (setf (processor-stopped-p processor) nil)
    (setf (processor-thread processor)
          (bt:make-thread
           (lambda ()
             (run processor :timeout timeout)
             (setf (processor-thread processor) nil))
           :initial-bindings `((*standard-output* . ,*standard-output*))
           :name "redqing processor"))
    processor))

(defgeneric stop (processor)
  (:method ((processor processor))
    (when (processor-stopped-p processor)
      (return-from stop nil))
    (vom:info "Exiting...")
    (setf (processor-stopped-p processor) t)
    t))

(defgeneric kill (processor)
  (:method ((processor processor))
    (stop processor)
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)))
    (setf (processor-thread processor) nil)
    t))

(defun decode-job (job-info)
  (let ((class (assoc "class" job-info :test #'string=))
        (args  (assoc "args"  job-info :test #'string=)))
    (unless (and class args)
      (error "Invalid job: ~S" job-info))
    (let ((class (read-from-string (cdr class))))
      (check-type class symbol)
      (let ((job (make-instance class)))
        (check-type job job)
        job))))

(defgeneric process-job (processor job-info)
  (:method ((processor processor) job-info)
    (let ((job (decode-job job-info))
          (args (cdr (assoc "args" job-info :test #'string=))))
      (vom:info "got: ~A ~S" job args)
      (apply #'perform-job processor job args))))

(defgeneric perform-job (processor job &rest args)
  (:method ((processor processor) job &rest args)
    (declare (ignore processor))
    (handler-bind ((error
                     (lambda (condition)
                       (vom:warn "Job ~A ~S failed with ~A" job args condition))))
      (apply #'perform job args))))
