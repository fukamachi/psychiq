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
           #:processor-stopped-p
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
           #:perform-job))
(in-package :redqing.worker.processor)

(defstruct (processor (:constructor %make-processor))
  (connection nil :type connection)
  (queues '() :type list)
  (manager nil)
  (thread nil)
  (stopped-p t)
  (timeout 5))

(defmethod print-object ((processor processor) stream)
  (print-unreadable-object (processor stream :type processor)
    (with-slots (queues stopped-p) processor
      (format stream "QUEUES: ~A / STATUS: ~:[RUNNING~;STOPPED~]"
              queues
              stopped-p))))

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
      until (processor-stopped-p processor)
      do (multiple-value-bind (job-info queue)
             (fetch-job processor)
           (if job-info
               (process-job processor queue job-info)
               (vom:debug "Timed out after ~D seconds"
                          (processor-timeout processor)))))))

(defgeneric finalize (processor)
  (:method ((processor processor))
    (setf (processor-stopped-p processor) t)
    (disconnect (processor-connection processor))
    (setf (processor-thread processor) nil)
    t))

(defgeneric start (processor)
  (:method ((processor processor))
    (setf (processor-stopped-p processor) nil)
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
    (when (processor-stopped-p processor)
      (return-from stop nil))
    (setf (processor-stopped-p processor) t)
    t))

(defgeneric kill (processor)
  (:method ((processor processor))
    (setf (processor-stopped-p processor) t)
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)
        (loop while (or (bt:thread-alive-p thread)
                        (processor-thread processor))
              do (sleep 0.1))))
    t))

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
