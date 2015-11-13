(in-package :cl-user)
(defpackage redqing.worker.manager
  (:use #:cl
        #:redqing.worker.processor)
  (:export #:manager
           #:make-manager
           #:start
           #:stop))
(in-package :redqing.worker.manager)

(defstruct (manager (:constructor %make-manager))
  (connection nil)
  (queues '())
  (children '())
  (lock (bt:make-lock))
  (stopped-p t))

(defun make-manager (&key connection queues (count 25))
  (let ((manager (%make-manager :connection connection :queues queues)))
    (setf (manager-children manager)
          (loop repeat count
                collect (make-processor :connection connection
                                        :queues queues
                                        :manager manager)))
    manager))

(defun processor-stopped (manager processor)
  (bt:with-lock-held ((manager-lock manager))
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))))

(defun processor-died (manager processor)
  (bt:with-lock-held ((manager-lock manager))
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))
    (unless (manager-stopped-p manager)
      (let ((new-processor
              (make-processor :connection (manager-connection manager)
                              :queues (manager-queues manager)
                              :manager manager)))
        (push new-processor (manager-children manager))
        (start new-processor)))))

(defmethod run :around ((processor processor) &key timeout)
  (declare (ignore timeout))
  (handler-case (call-next-method)
    (error ()
      (processor-died (processor-manager processor) processor)))
  (vom:info "Shutting down..."))

(defmethod perform-job :around ((processor processor) job &rest args)
  (handler-bind ((error
                   (lambda (condition)
                     (vom:warn "Job ~A ~S failed with ~A" job args condition))))
    (call-next-method)))

(defmethod start ((manager manager))
  (setf (manager-stopped-p manager) nil)
  (map nil #'start (manager-children manager))
  manager)

(defmethod stop ((manager manager))
  (when (manager-stopped-p manager)
    (return-from stop nil))

  (setf (manager-stopped-p manager) t)
  (vom:info "Terminating quiet workers")
  (map nil #'stop (manager-children manager))
  t)
