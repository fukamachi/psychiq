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
  (:import-from #:psychiq.middleware.logging
                #:*psychiq-middleware-logging*)
  (:import-from #:alexandria
                #:shuffle)
  (:export #:processor
           #:make-processor
           #:processor-id
           #:processor-status
           #:processor-manager
           #:processor-connection
           #:processor-timeout
           #:processor-processing
           #:run
           #:start
           #:stop
           #:kill
           #:wait-for
           #:finalize
           #:fetch-job
           #:process-job
           #:perform-job))
(in-package :psychiq.launcher.processor)

(defstruct (processor (:constructor %make-processor))
  (id (generate-random-id 9))
  (connection nil :type connection)
  (queues '() :type list)
  (manager nil)
  (thread nil)
  (status :stopped)
  (timeout 5)
  down
  (processing nil))

(defmethod print-object ((processor processor) stream)
  (print-unreadable-object (processor stream :type t)
    (with-slots (queues status) processor
      (format stream "QUEUES: ~A / STATUS: ~A"
              queues
              status))))

(defun make-processor (&key (host *default-redis-host*) (port *default-redis-port*) db
                         queues manager (timeout 5))
  (unless (and (listp queues)
               queues)
    (error ":queues must be a list containing at least one queue name"))
  (let ((conn (make-connection :host host :port port :db db)))
    (%make-processor :connection conn :queues queues :manager manager :timeout timeout)))

(defgeneric fetch-job (processor)
  (:method ((processor processor))
    (handler-bind ((redis:redis-connection-error
                     (lambda (e)
                       (unless (processor-down processor)
                         (setf (processor-down processor)
                               (get-internal-real-time))
                         (vom:error "Error fetching job (~S): ~A"
                                    (class-name (class-of e))
                                    e)
                         (disconnect (processor-connection processor)))
                       (sleep 1)
                       (return-from fetch-job nil))))
      (multiple-value-bind (job-info queue)
          (with-connection (processor-connection processor)
            (dequeue-from-queue (shuffle
                                 (copy-seq (processor-queues processor)))
                                :timeout (processor-timeout processor)))
        (when (processor-down processor)
          (vom:info "Redis is online, ~A sec downtime"
                    (/ (- (get-internal-real-time)
                           (processor-down processor))
                       1000.0))
          (setf (processor-down processor) nil))
        (if job-info
            (progn
              (vom:debug "Found job on ~A" queue)
              (values job-info queue))
            nil)))))

(defgeneric run (processor)
  (:method ((processor processor))
    (loop
      while (eq (processor-status processor) :running)
      do (multiple-value-bind (job-info queue)
             (fetch-job processor)
           (when job-info
             (process-job processor queue job-info))))))

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
          (bt2:make-thread
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
      (when (and (bt2:threadp thread)
                 (bt2:thread-alive-p thread))
        (bt2:destroy-thread thread)))
    t))

(defgeneric wait-for (object)
  (:method ((processor processor))
    (let ((thread (processor-thread processor)))
      (when (bt2:threadp thread)
        (ignore-errors (bt2:join-thread thread))))
    t))

(defgeneric process-job (processor queue job-info)
  (:method ((processor processor) queue job-info)
    (vom:debug "Got: ~S (ID: ~A)"
               (aget job-info "class")
               (aget job-info "jid"))
    (handler-bind ((error
                     (lambda (condition)
                       (vom:warn "Job ~A failed with ~S: ~A"
                                 (aget job-info "class")
                                 (class-name (class-of condition))
                                 condition))))
      (let ((worker (decode-job job-info)))
        ;; Applying default middlewares
        (with-connection (processor-connection processor)
          (funcall
           (reduce #'funcall
                   (list *psychiq-middleware-retry-jobs*
                         *psychiq-middleware-logging*)
                   :initial-value
                   (lambda (worker job-info queue)
                     (apply #'perform-job
                            processor
                            queue
                            worker
                            (aget job-info "args")))
                   :from-end t)
           worker job-info queue))))))

(defgeneric perform-job (processor queue worker &rest args)
  (:method ((processor processor) queue worker &rest args)
    (declare (ignore queue))
    (with-connection (processor-connection processor)
      (apply #'perform worker args))))
