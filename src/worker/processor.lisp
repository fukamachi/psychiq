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
           #:processor-stopped-p
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
  (thread nil)
  (stopped-p nil))

(defgeneric fetch-job (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (let ((ret
            (with-redis-connection (processor-connection processor)
              (apply #'red:blpop
                     (mapcar (lambda (queue)
                               (redis-key "queue" queue))
                             ;; TODO: allow to shuffle the queues
                             (processor-queues processor))
                     (list timeout)))))
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
               (process-job job-info)
               (vom:debug "Timed out after ~D seconds" timeout))))))

(defgeneric start (processor)
  (:method ((processor processor))
    (vom:info "Starting...")
    (setf (processor-stopped-p processor) nil)
    (setf (processor-thread processor)
          (bt:make-thread
           (lambda ()
             (run processor))
           :initial-bindings `((*standard-output* . ,*standard-output*))))
    processor))

(defgeneric stop (processor)
  (:method ((processor processor))
    (vom:info "Exiting...")
    (setf (processor-stopped-p processor) t)))

(defgeneric kill (processor)
  (:method ((processor processor))
    (stop processor)
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)))
    (setf (processor-thread processor) nil)
    processor))

(defun decode-job (job-info)
  (let ((class (cdr (assoc "class" job-info :test #'string=)))
        (args  (cdr (assoc "args"  job-info :test #'string=))))
    (unless (and class args)
      (error "Invalid job: ~S" job-info))
    (let ((class (read-from-string class)))
      (check-type class symbol)
      (let ((job (make-instance class)))
        (check-type job job)
        job))))

(defgeneric process-job (processor job-info)
  (:method ((processor processor) job-info)
    (declare (ignore processor))
    (let ((job (decode-job job-info))
          (args (cdr (assoc "args" job-info :test #'string=))))
      (vom:info "got: ~A ~S" job args)
      (apply #'perform-job processor job args))))

(defgeneric perform-job (processor job &rest args)
  (:method ((processor processor) job &rest args)
    (declare (ignore processor))
    (apply #'perform job args)))
