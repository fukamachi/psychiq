(in-package :cl-user)
(defpackage redqing.worker.manager
  (:use #:cl
        #:redqing.worker.processor)
  (:import-from #:alexandria
                #:when-let)
  (:export #:manager
           #:make-manager
           #:start
           #:stop
           #:kill))
(in-package :redqing.worker.manager)

(defstruct (manager (:constructor %make-manager))
  host
  port
  (queues '())
  (children '())
  (lock (bt:make-recursive-lock))
  (stopped-p t))

(defmethod print-object ((manager manager) stream)
  (print-unreadable-object (manager stream :type manager)
    (with-slots (queues children stopped-p) manager
      (format stream "QUEUES: ~A / COUNT: ~A / STATUS: ~:[RUNNING~;STOPPED~]"
              queues
              (length children)
              stopped-p))))

(defun make-manager (&key (host "localhost") (port 6379) queues (count 25))
  (let ((manager (%make-manager :host host :port port :queues queues)))
    (setf (manager-children manager)
          (loop repeat count
                collect (make-processor :host host
                                        :port port
                                        :queues queues
                                        :manager manager)))
    manager))

(defun processor-stopped (manager processor)
  (bt:with-recursive-lock-held ((manager-lock manager))
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))
    (when (null (manager-children manager))
      (setf (manager-stopped-p manager) t)))
  (values))

(defun processor-died (manager processor)
  (bt:with-recursive-lock-held ((manager-lock manager))
    (stop processor)
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))
    (unless (manager-stopped-p manager)
      (vom:debug "Adding a new processor...")
      (let ((new-processor
              (make-processor :host (manager-host manager)
                              :port (manager-port manager)
                              :queues (manager-queues manager)
                              :manager manager)))
        (push new-processor (manager-children manager))
        (start new-processor))))
  (values))

(defmethod run :around ((processor processor) &key timeout)
  (declare (ignore timeout))
  (handler-case (call-next-method)
    (error (e)
      (vom:warn "Processor died with ~S: ~A" (class-name (class-of e)) e)
      (when-let (manager (processor-manager processor))
        (processor-died manager processor))))
  (vom:debug "Shutting down a processor..."))

(defmethod stop :after ((processor processor))
  (when-let (manager (processor-manager processor))
    (processor-stopped manager processor)))

(defmethod start ((manager manager) &rest args &key timeout)
  (declare (ignore timeout))
  (setf (manager-stopped-p manager) nil)
  (map nil (lambda (processor)
             (apply #'start processor args))
       (manager-children manager))
  manager)

(defmethod stop ((manager manager))
  (when (manager-stopped-p manager)
    (return-from stop nil))

  (setf (manager-stopped-p manager) t)
  (vom:info "Terminating quiet processors...")
  (map nil #'stop (manager-children manager))
  (vom:info "Exiting...")
  t)

(defmethod kill ((manager manager))
  (when (manager-stopped-p manager)
    (return-from kill nil))

  (setf (manager-stopped-p manager) t)
  (map nil #'kill (manager-children manager))
  (vom:info "Exiting...")
  t)
