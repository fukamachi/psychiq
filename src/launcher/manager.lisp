(in-package :cl-user)
(defpackage psychiq.launcher.manager
  (:use #:cl
        #:psychiq.util
        #:psychiq.launcher.processor
        #:psychiq.specials)
  (:import-from #:psychiq.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:local-time
                #:today
                #:format-timestring)
  (:import-from #:alexandria
                #:when-let)
  (:export #:manager
           #:make-manager
           #:manager-stopped-p
           #:manager-stat-processed
           #:manager-stat-failed
           #:start
           #:stop
           #:kill))
(in-package :psychiq.launcher.manager)

(defstruct (manager (:constructor
                        %make-manager
                      (&key host port queues count (lock (bt:make-recursive-lock))
                       &aux
                         (stat-processed (make-thread-safe-variable 0 :lock lock))
                         (stat-failed    (make-thread-safe-variable 0 :lock lock)))))
  host
  port
  (queues '())
  (count 25)
  (children '())
  (lock (bt:make-recursive-lock))
  (stopped-p t)

  ;; stats
  stat-processed
  stat-failed

  heartbeat-thread

  make-processor-fn)

(defun make-manager (&key (host *default-redis-host*) (port *default-redis-port*) queues (count 25) (timeout 5))
  (let ((manager (%make-manager :host host :port port :queues queues
                                :count count)))
    (setf (manager-make-processor-fn manager)
          (lambda ()
            (make-processor :host host
                            :port port
                            :queues queues
                            :manager manager
                            :timeout timeout)))
    manager))

(defun make-child-processors (manager)
  (bt:with-recursive-lock-held ((manager-lock manager))
    (setf (manager-children manager)
          (loop repeat (manager-count manager)
                collect (funcall (manager-make-processor-fn manager))))))

(defun processor-stopped (manager processor)
  (bt:with-recursive-lock-held ((manager-lock manager))
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))
    (when (null (manager-children manager))
      (setf (manager-stopped-p manager) t)))
  (values))

(defun processor-died (manager processor e)
  (bt:with-recursive-lock-held ((manager-lock manager))
    (stop processor)
    (setf (manager-children manager)
          (delete processor (manager-children manager) :test #'eq))
    (unless (manager-stopped-p manager)
      (vom:warn "Processor died with ~S: ~A" (class-name (class-of e)) e)
      (vom:debug "Adding a new processor...")
      (let ((new-processor
              (funcall (manager-make-processor-fn manager))))
        (push new-processor (manager-children manager))
        (start new-processor))))
  (values))

(defmethod run :around ((processor processor))
  (handler-case (call-next-method)
    (error (e)
      (when-let (manager (processor-manager processor))
        (processor-died manager processor e))))
  (vom:debug "Shutting down a processor..."))

(defmethod process-job :around ((processor processor) queue job-info)
  (unwind-protect
       (handler-bind ((error
                        (lambda (condition)
                          (declare (ignore condition))
                          (when-let (manager (processor-manager processor))
                            (incf (get-value (manager-stat-failed manager)))))))
         (call-next-method))
    (when-let (manager (processor-manager processor))
      (incf (get-value (manager-stat-processed manager))))))

(defmethod finalize :after ((processor processor))
  (when-let (manager (processor-manager processor))
    (processor-stopped manager processor)))

(defmethod start ((manager manager))
  (setf (manager-stopped-p manager) nil)
  (make-child-processors manager)
  (map nil #'start (manager-children manager))
  (setf (manager-heartbeat-thread manager)
        (bt:make-thread
         (lambda () (start-heartbeat manager))))
  manager)

(defmethod stop ((manager manager))
  (when (manager-stopped-p manager)
    (return-from stop nil))

  (setf (manager-stopped-p manager) t)
  (vom:info "Terminating quiet processors...")
  (map nil #'stop (manager-children manager))
  (let ((thread (manager-heartbeat-thread manager)))
    (when (bt:threadp thread)
      (bt:destroy-thread thread)))
  (vom:info "Exiting...")
  t)

(defmethod kill ((manager manager))
  (setf (manager-stopped-p manager) t)
  (vom:info "Terminating all processors...")
  (map nil #'kill (manager-children manager))
  (let ((thread (manager-heartbeat-thread manager)))
    (when (bt:threadp thread)
      (bt:destroy-thread thread)))
  (vom:info "Exiting...")
  t)

(defun start-heartbeat (manager)
  (let ((conn (connect :host (manager-host manager)
                       :port (manager-port manager))))
    (unwind-protect
         (with-connection conn
           (loop
             (heartbeat manager)
             (sleep 5)))
      (vom:info "Heartbeat stopping...")
      (disconnect conn)
      (setf (manager-heartbeat-thread manager) nil))))

(defun heartbeat (manager)
  (let ((processed (reset-value (manager-stat-processed manager)))
        (failed    (reset-value (manager-stat-failed manager)))
        (today (format-timestring nil (today)
                                  :format '((:year 4) #\- (:month 2) #\- (:day 2)))))
    (handler-case
        (redis:with-pipelining
          (red:incrby (redis-key "stat" "processed")
                      processed)
          (red:incrby (redis-key "stat" "processed" today)
                      processed)
          (red:incrby (redis-key "stat" "failed")
                      failed)
          (red:incrby (redis-key "stat" "failed" today)
                      failed))
      (error (e)
        ;; Ignore all redis/network issues
        (vom:error "heartbeat: ~A" e)
        ;; Don't lose the counts if there was a network issue
        (setf (get-value (manager-stat-processed manager)) processed)
        (setf (get-value (manager-stat-failed manager)) failed)))))
