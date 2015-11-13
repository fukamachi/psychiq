(in-package :cl-user)
(defpackage redqing.worker.child
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
  (:export #:worker
           #:start-worker
           #:shutdown-worker
           #:kill-worker))
(in-package :redqing.worker.child)

(defstruct worker
  (connection nil :type (or null connection))
  (queues '() :type list)
  (thread nil)
  (shutdown-p nil))

(defun fetch-job (worker &key (timeout 5))
  (let ((ret
          (with-redis-connection (worker-connection worker)
            (apply #'red:blpop
                   (mapcar (lambda (queue)
                             (redis-key "queue" queue))
                           (worker-queues worker))
                   (list timeout)))))
    (if ret
        (destructuring-bind (queue payload) ret
          (vom:debug "Found job on ~A" queue)
          (decode (connection-coder (worker-connection worker))
                  payload))
        nil)))

(defun run (worker &key (timeout 5))
  (handler-case
      (loop
        until (worker-shutdown-p worker)
        do (let ((job-info (fetch-job worker :timeout timeout)))
             (if job-info
                 (process-job job-info)
                 (vom:debug "Timed out after ~D seconds" timeout))))
    ;; TODO: error handling
    (error ()
      ))
  (vom:info "Shutting down..."))

(defun start-worker (worker)
  (vom:info "Starting...")
  (setf (worker-shutdown-p worker) nil)
  (setf (worker-thread worker)
        (bt:make-thread
         (lambda ()
           (run worker))
         :initial-bindings `((*standard-output* . ,*standard-output*))))
  worker)

(defun shutdown-worker (worker)
  (vom:info "Exiting...")
  (setf (worker-shutdown-p worker) t))

(defun kill-worker (worker)
  (shutdown-worker worker)
  (let ((thread (worker-thread worker)))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (bt:destroy-thread thread)))
  (setf (worker-thread worker) nil)
  worker)

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

(defun process-job (job-info)
  (let ((job (decode-job job-info))
        (args (cdr (assoc "args" job-info :test #'string=))))
    (vom:info "got: ~A ~S" job args)
    (handler-case
        (apply #'perform job args)
      ;; TODO: error handling
      (error ()
        ))))
