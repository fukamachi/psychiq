(in-package :cl-user)
(defpackage redqing.worker.processor
  (:use #:cl
        #:redqing.util
        #:redqing.specials)
  (:import-from #:redqing.connection
                #:connection
                #:make-connection
                #:ensure-connected
                #:disconnect
                #:with-connection)
  (:import-from #:redqing.job
                #:perform
                #:decode-job)
  (:import-from #:redqing.middleware.retry-jobs
                #:*redqing-middleware-retry-jobs*)
  (:import-from #:redqing.coder
                #:decode-object)
  (:export #:processor
           #:make-processor
           #:processor-status
           #:processor-manager
           #:processor-connection
           #:processor-timeout
           #:run
           #:start
           #:stop
           #:kill
           #:finalize
           #:fetch-job
           #:process-job
           #:perform-job
           #:wait-processor-ends))
(in-package :redqing.worker.processor)

(defstruct (processor (:constructor %make-processor))
  (connection nil :type connection)
  (queues '() :type list)
  (manager nil)
  (thread nil)
  (status :stopped)
  (timeout 5))

(defmethod print-object ((processor processor) stream)
  (print-unreadable-object (processor stream :type processor)
    (with-slots (queues status) processor
      (format stream "QUEUES: ~A / STATUS: ~A"
              queues
              status))))

(defun make-processor (&key (host *default-redis-host*) (port *default-redis-port*) queues manager (timeout 5))
  (unless (and (listp queues)
               queues)
    (error ":queues must be a list containing at least one queue name"))
  (let ((conn (make-connection :host host :port port)))
    (%make-processor :connection conn :queues queues :manager manager :timeout timeout)))

(defgeneric fetch-job (processor)
  (:method ((processor processor))
    (let ((ret
            (with-connection (processor-connection processor)
              (apply #'red:blpop
                     (nconc
                      (mapcar (lambda (queue)
                                (redis-key "queue" queue))
                              ;; TODO: allow to shuffle the queues
                              (processor-queues processor))
                      (list (processor-timeout processor)))))))
      (if ret
          (destructuring-bind (queue payload) ret
            (vom:debug "Found job on ~A" queue)
            (values
             (decode-object payload)
             (omit-redis-prefix queue "queue")))
          nil))))

(defgeneric run (processor)
  (:method ((processor processor))
    (loop
      while (eq (processor-status processor) :running)
      do (multiple-value-bind (job-info queue)
             (fetch-job processor)
           (if job-info
               (process-job processor queue job-info)
               (vom:debug "Timed out after ~D seconds"
                          (processor-timeout processor)))))))

(defgeneric finalize (processor)
  (:method ((processor processor))
    (disconnect (processor-connection processor))
    (setf (processor-thread processor) nil)
    (setf (processor-status processor) :stopped)
    t))

(defgeneric start (processor)
  (:method ((processor processor))
    (setf (processor-status processor) :running)
    (setf (processor-thread processor)
          (bt:make-thread
           (lambda ()
             (unwind-protect
                  (progn
                    (ensure-connected (processor-connection processor))
                    (run processor))
               (finalize processor)))
           :initial-bindings `((*standard-output* . ,*standard-output*)
                               (*error-output* . ,*error-output*))
           :name "redqing processor"))
    processor))

(defgeneric stop (processor)
  (:method ((processor processor))
    (unless (eq (processor-status processor) :running)
      (return-from stop nil))
    (setf (processor-status processor) :stopping)
    t))

(defgeneric kill (processor &optional wait)
  (:method ((processor processor) &optional (wait t))
    (setf (processor-status processor) :stopping)
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)
        (when wait
          (wait-processor-ends processor)
          (sleep 2))))
    t))

(defun wait-processor-ends (processor)
  (let ((thread (processor-thread processor)))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (loop while (bt:thread-alive-p thread)
            do (sleep 0.1)))))

(defgeneric process-job (processor queue job-info)
  (:method ((processor processor) queue job-info)
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
