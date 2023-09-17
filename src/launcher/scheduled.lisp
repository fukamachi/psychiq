(in-package :cl-user)
(defpackage psychiq.launcher.scheduled
  (:use #:cl
        #:psychiq.util
        #:psychiq.specials)
  (:import-from #:psychiq.connection
                #:with-connection
                #:make-connection
                #:disconnect)
  (:import-from #:psychiq.queue
                #:enqueue-to-queue)
  (:import-from #:psychiq.coder
                #:decode-object)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:scheduled
           #:scheduled-status
           #:scheduled-thread
           #:start
           #:stop
           #:kill
           #:make-scheduled))
(in-package :psychiq.launcher.scheduled)

(defstruct (scheduled (:constructor %make-scheduled))
  connection
  thread
  (status :stopped))

(defun make-scheduled (&key (host *default-redis-host*) (port *default-redis-port*) db)
  (let ((conn (make-connection :host host :port port :db db)))
    (%make-scheduled :connection conn)))

(defun start (scheduled)
  (when (eq (scheduled-status scheduled) :running)
    (error "Scheduled thread is already running"))

  (setf (scheduled-status scheduled) :running)
  (let* ((conn (scheduled-connection scheduled))
         (thread
           (bt2:make-thread
            (lambda ()
              (unwind-protect
                   (loop while (eq (scheduled-status scheduled) :running)
                         do (handler-case (with-connection conn
                                            (enqueue-jobs (timestamp-to-unix (now))))
                              (redis:redis-connection-error (e)
                                (vom:error "polling scheduled: ~A" e)
                                (disconnect conn)))
                            (sleep (scaled-poll-interval)))
                (disconnect (scheduled-connection scheduled))
                (setf (scheduled-thread scheduled) nil)
                (setf (scheduled-status scheduled) :stopped)))
            :initial-bindings `((*standard-output* . ,*standard-output*)
                                (*error-output* . ,*error-output*))
            :name "psychiq scheduled")))
    (setf (scheduled-thread scheduled) thread))
  scheduled)

(defun scaled-poll-interval ()
  (let* ( ;; Should be changed to the number of Psychiq processes
         (process-count 1)
         (poll-interval-average (* process-count 2)))
    (+ (* poll-interval-average (random 1.0))
       (/ poll-interval-average 2))))

(defun stop (scheduled)
  (unless (eq (scheduled-status scheduled) :running)
    (return-from stop nil))

  (setf (scheduled-status scheduled) :stopping))

(defun kill (scheduled)
  (setf (scheduled-status scheduled) :stopping)
  (let ((thread (scheduled-thread scheduled)))
    (when (and (bt2:threadp thread)
               (bt2:thread-alive-p thread))
      (bt2:destroy-thread thread)))
  t)

(defun enqueue-jobs (now)
  (dolist (queue '("retry" "schedule"))
    (loop for payload = (first
                         (red:zrangebyscore (redis-key queue)
                                            "-inf"
                                            now
                                            :limit '(0 . 1)))
          while payload
          do (red:zrem (redis-key queue) payload)
             (let* ((job-info (decode-object payload))
                    (queue (or (aget job-info "queue") *default-queue-name*)))
               (enqueue-to-queue queue job-info)
               (vom:debug "Enqueued to ~A: ~S" queue job-info)))))
