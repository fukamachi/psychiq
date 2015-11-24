(in-package :cl-user)
(defpackage psychiq.launcher.processor
  (:use #:cl
        #:psychiq.util
        #:psychiq.specials)
  (:import-from #:psychiq.connection
                #:*connection*
                #:connection
                #:make-connection
                #:ensure-connected
                #:disconnect
                #:with-connection)
  (:import-from #:psychiq.worker
                #:perform
                #:decode-job)
  (:import-from #:psychiq.queue
                #:dequeue-from-queue)
  (:import-from #:psychiq.middleware.retry-jobs
                #:*psychiq-middleware-retry-jobs*)
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
           #:perform-job))
(in-package :psychiq.launcher.processor)

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
    (multiple-value-bind (job-info queue)
        ;; TODO: allow to shuffle the queues
        (with-connection (processor-connection processor)
          (dequeue-from-queue (processor-queues processor)
                              :timeout (processor-timeout processor)))
      (if job-info
          (progn
            (vom:debug "Found job on ~A" queue)
            (values job-info queue))
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
           :name "psychiq processor"))
    processor))

(defgeneric stop (processor)
  (:method ((processor processor))
    (unless (eq (processor-status processor) :running)
      (return-from stop nil))
    (setf (processor-status processor) :stopping)
    t))

(defgeneric kill (processor)
  (:method ((processor processor))
    (setf (processor-status processor) :stopping)
    (let ((thread (processor-thread processor)))
      (when (and (bt:threadp thread)
                 (bt:thread-alive-p thread))
        (bt:destroy-thread thread)))
    t))

(defgeneric process-job (processor queue job-info)
  (:method ((processor processor) queue job-info)
    (vom:info "Got: ~S (ID: ~A)"
              (aget job-info "class")
              (aget job-info "jid"))
    (handler-bind ((error
                     (lambda (condition)
                       (vom:warn "Job ~A failed with ~S: ~A"
                                 (aget job-info "class")
                                 (class-name (class-of condition)) condition))))
      (multiple-value-bind (worker args)
          (decode-job job-info)
        ;; Applying default middlewares
        (funcall
         (funcall *psychiq-middleware-retry-jobs*
                  (lambda (worker args)
                    (apply #'perform-job processor queue worker args)))
         (processor-connection processor) worker args job-info queue)))))

(defgeneric perform-job (processor queue worker &rest args)
  (:method ((processor processor) queue worker &rest args)
    (declare (ignore queue))
    (with-connection (processor-connection processor)
      (apply #'perform worker args))))
