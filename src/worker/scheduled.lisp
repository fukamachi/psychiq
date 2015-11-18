(in-package :cl-user)
(defpackage redqing.worker.scheduled
  (:use #:cl
        #:redqing.util
        #:redqing.specials)
  (:import-from #:redqing.connection
                #:with-connection
                #:make-connection
                #:disconnect)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:import-from #:redqing.coder
                #:decode-object)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:scheduled
           #:scheduled-status
           #:start
           #:stop
           #:kill
           #:make-scheduled))
(in-package :redqing.worker.scheduled)

(defstruct (scheduled (:constructor %make-scheduled))
  connection
  thread
  (status :stopped))

(defun make-scheduled (&key (host *default-redis-host*) (port *default-redis-port*))
  (let ((conn (make-connection :host host :port port)))
    (%make-scheduled :connection conn)))

(defun start (scheduled)
  (when (eq (scheduled-status scheduled) :running)
    (error "Scheduled thread is already running"))

  (setf (scheduled-status scheduled) :running)
  (let* ((conn (scheduled-connection scheduled))
         (thread
           (bt:make-thread
            (lambda ()
              (unwind-protect
                   (loop while (eq (scheduled-status scheduled) :running)
                         do (handler-case (with-connection conn
                                            (enqueue-jobs (timestamp-to-unix (now))))
                              (error (e)
                                (vom:error "~A" e)))
                            (sleep (scaled-poll-interval)))
                (disconnect (scheduled-connection scheduled))
                (setf (scheduled-thread scheduled) nil)
                (setf (scheduled-status scheduled) :stopped)))
            :initial-bindings `((*standard-output* . ,*standard-output*)
                                (*error-output* . ,*error-output*))
            :name "redqing scheduled")))
    (setf (scheduled-thread scheduled) thread))
  scheduled)

(defun scaled-poll-interval ()
  (let* ( ;; Should be changed to the number of Red Qing processes
         (process-count 1)
         (poll-interval-average (* process-count 2)))
    (+ (* poll-interval-average (random 1.0))
       (/ poll-interval-average 2))))

(defun stop (scheduled)
  (unless (eq (scheduled-status scheduled) :running)
    (return-from stop nil))

  (setf (scheduled-status scheduled) :stopping))

(defun kill (scheduled &optional (wait t))
  (setf (scheduled-status scheduled) :stopping)
  (let ((thread (scheduled-thread scheduled)))
    (when (and (bt:threadp thread)
               (bt:thread-alive-p thread))
      (bt:destroy-thread thread)
      (when wait
        (loop while (or (bt:thread-alive-p thread)
                        (not (eq (scheduled-status scheduled) :stopped)))
              do (sleep 0.1))
        (sleep 3))))
  t)

(defun enqueue-jobs (now)
  (loop for payload = (first
                       (red:zrangebyscore (redis-key "retry")
                                          "-inf"
                                          now
                                          :limit '(0 . 1)))
        while payload
        do (red:zrem (redis-key "retry") payload)
           (let* ((job-info (decode-object payload))
                  (queue (or (aget job-info "queue") *default-queue-name*)))
             (enqueue-to-queue queue job-info)
             (vom:debug "Enqueued to ~A: ~S" queue job-info))))
