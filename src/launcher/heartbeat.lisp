(in-package :cl-user)
(defpackage psychiq.launcher.heartbeat
  (:use #:cl
        #:psychiq.util)
  (:import-from #:psychiq.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:psychiq.coder
                #:encode-object)
  (:import-from #:psychiq.launcher.manager
                #:manager-stat-processed
                #:manager-stat-failed
                #:manager-count
                #:manager-children
                #:manager-queues)
  (:import-from #:psychiq.launcher.processor
                #:processor-id
                #:processor-processing)
  (:import-from #:local-time
                #:now
                #:today
                #:format-timestring
                #:timestamp-to-unix)
  (:export #:heartbeat
           #:make-heartbeat
           #:start
           #:stop
           #:kill))
(in-package :psychiq.launcher.heartbeat)

(defstruct heartbeat
  host
  port
  thread
  (stopped-p t)
  manager)

(defun run (heartbeat)
  (let* ((conn (connect :host (heartbeat-host heartbeat)
                        :port (heartbeat-port heartbeat)))
         (manager (heartbeat-manager heartbeat))
         (machine-identity (machine-identity))
         (json (jojo:to-json
                `(("hostname" . ,(uiop:hostname))
                  ("started_at" . ,(timestamp-to-unix (now)))
                  ("pid" . ,(getpid))
                  ("concurrency" . ,(manager-count manager))
                  ("queues" . ,(remove-duplicates (manager-queues manager)
                                                  :test #'equal
                                                  :from-end t))
                  ("identity" . ,machine-identity))
                :from :alist)))
    (unwind-protect
         (loop until (heartbeat-stopped-p heartbeat) do
           (handler-case
               (with-connection conn
                 (heartbeat heartbeat machine-identity json))
             (redis:redis-connection-error (e)
               (vom:error "heartbeat: ~A" e)
               (disconnect conn)))
           (sleep 5))
      (disconnect conn))))

(defun clear-heartbeat (heartbeat)
  (let ((conn (connect :host (heartbeat-host heartbeat)
                       :port (heartbeat-port heartbeat))))
    (with-connection conn
      (redis:with-pipelining
        (red:srem (redis-key "processes") (machine-identity))))
    (disconnect conn)))

(defun start (heartbeat)
  (setf (heartbeat-stopped-p heartbeat) nil)
  (setf (heartbeat-thread heartbeat)
        (bt:make-thread
         (lambda () (run heartbeat))
         :initial-bindings `((*standard-output* . ,*standard-output*)
                             (*error-output* . ,*error-output*))
         :name "psychiq heartbeat"))
  heartbeat)

(defun stop (heartbeat)
  (when (heartbeat-stopped-p heartbeat)
    (return-from stop nil))

  (setf (heartbeat-stopped-p heartbeat) t)
  (vom:info "Heartbeat stopping...")
  (clear-heartbeat heartbeat)
  t)

(defun kill (heartbeat)
  (setf (heartbeat-stopped-p heartbeat) t)

  (let ((thread (heartbeat-thread heartbeat)))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (vom:info "Heartbeat stopping immediately...")
      (bt:destroy-thread (heartbeat-thread heartbeat))))

  (setf (heartbeat-thread heartbeat) nil)
  t)

(defun heartbeat (heartbeat machine-identity json)
  (let* ((manager (heartbeat-manager heartbeat))
         (processed (reset-value (manager-stat-processed manager)))
         (failed    (reset-value (manager-stat-failed manager)))
         (today (format-timestring nil (today)
                                   :format '((:year 4) #\- (:month 2) #\- (:day 2)))))
    (handler-bind ((error
                     (lambda (e)
                       (declare (ignore e))
                       ;; Don't lose the counts if there was a network issue
                       (setf (get-value (manager-stat-processed manager)) processed)
                       (setf (get-value (manager-stat-failed manager)) failed))))
      (redis:with-pipelining
        (red:incrby (redis-key "stat" "processed")
                    processed)
        (red:incrby (redis-key "stat" "processed" today)
                    processed)
        (red:incrby (redis-key "stat" "failed")
                    failed)
        (red:incrby (redis-key "stat" "failed" today)
                    failed)

        (red:del (redis-key machine-identity "workers"))
        (dolist (processor (manager-children manager))
          (when (processor-processing processor)
            (red:hset (redis-key machine-identity "workers")
                      (processor-id processor)
                      (encode-object (processor-processing processor))))))

      (redis:with-pipelining
        (red:sadd (redis-key "processes")
                  machine-identity)
        (red:hmset (redis-key machine-identity)
                   "info" json
                   "busy" (count-if #'processor-processing
                                    (manager-children manager))
                   "beat" (timestamp-to-unix (now)))
        (red:expire (redis-key machine-identity)
                    60)))))
