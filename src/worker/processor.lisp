(in-package :cl-user)
(defpackage redqing.worker.processor
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:make-connection
                #:ensure-connected
                #:disconnect
                #:with-redis-connection)
  (:import-from #:redqing.job
                #:perform
                #:decode-job)
  (:import-from #:redqing.middleware.retry-jobs
                #:*redqing-middleware-retry-jobs*)
  (:import-from #:redqing.redis
                #:redis-key)
  (:import-from #:redqing.coder
                #:decode-object)
  (:import-from #:redqing.util.assoc
                #:aget)
  (:export #:processor
           #:make-processor
           #:processor-stopped-p
           #:processor-manager
           #:processor-connection
           #:run
           #:start
           #:stop
           #:kill
           #:fetch-job
           #:process-job
           #:perform-job))
(in-package :redqing.worker.processor)

(defstruct (processor (:constructor %make-processor))
  (connection nil :type connection)
  (queues '() :type list)
  (manager nil)
  (thread nil)
  (stopped-p t))

(defmethod print-object ((processor processor) stream)
  (print-unreadable-object (processor stream :type processor)
    (with-slots (queues stopped-p) processor
      (format stream "QUEUES: ~A / STATUS: ~:[RUNNING~;STOPPED~]"
              queues
              stopped-p))))

(defun make-processor (&key (host "localhost") (port 6379) queues manager)
  (unless (and (listp queues)
               queues)
    (error ":queues must be a list containing at least one queue name"))
  (let ((conn (make-connection :host host :port port)))
    (%make-processor :connection conn :queues queues :manager manager)))

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
            (values
             (decode-object payload)
             queue))
          nil))))

(defgeneric run (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (loop
      until (processor-stopped-p processor)
      do (multiple-value-bind (job-info queue)
             (fetch-job processor :timeout timeout)
           (if job-info
               (process-job processor queue job-info)
               (vom:debug "Timed out after ~D seconds" timeout))))))

(defgeneric start (processor &key timeout)
  (:method ((processor processor) &key (timeout 5))
    (setf (processor-stopped-p processor) nil)
    (setf (processor-thread processor)
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (progn
                    (ensure-connected (processor-connection processor))
                    (run processor :timeout timeout))
               (setf (processor-thread processor) nil)
               (stop processor)
               (disconnect (processor-connection processor))))
           :initial-bindings `((*standard-output* . ,*standard-output*)
                               (*error-output* . ,*error-output*))
           :name "redqing processor"))
    processor))

(defgeneric stop (processor)
  (:method ((processor processor))
    (when (processor-stopped-p processor)
      (return-from stop nil))
    (setf (processor-stopped-p processor) t)
    t))

(defgeneric kill (processor)
  (:method ((processor processor))
    (when (processor-stopped-p processor)
      (return-from kill nil))
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)))
    (setf (processor-thread processor) nil)
    (setf (processor-stopped-p processor) t)
    t))

(defgeneric process-job (processor queue job-info)
  (:method ((processor processor) queue job-info)
    (declare (ignore queue))
    (let ((job (decode-job job-info))
          (args (aget job-info "args")))
      (vom:info "got: ~A ~S" job args)
      (handler-bind ((error
                       (lambda (condition)
                         (vom:warn "Job ~A ~S failed with ~S: ~A"
                                   job args (class-name (class-of condition)) condition))))
        ;; Applying default middlewares
        (funcall
         (funcall *redqing-middleware-retry-jobs*
                  (lambda (job args)
                    (apply #'perform-job processor queue job args)))
         (processor-connection processor) job queue args)))))

(defgeneric perform-job (processor queue job &rest args)
  (:method ((processor processor) queue job &rest args)
    (declare (ignore processor queue))
    (apply #'perform job args)))
