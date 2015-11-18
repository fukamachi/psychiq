(in-package :cl-user)
(defpackage redqing.scheduled
  (:use #:cl)
  (:import-from #:redqing.connection
                #:with-redis-connection
                #:make-connection
                #:ensure-connected
                #:disconnect)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:import-from #:redqing.coder
                #:decode-object)
  (:import-from #:redqing.redis
                #:redis-key)
  (:import-from #:redqing.util.assoc
                #:aget)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:scheduled
           #:scheduled-stopped-p
           #:start
           #:stop
           #:kill
           #:make-scheduled))
(in-package :redqing.scheduled)

(defstruct (scheduled (:constructor %make-scheduled))
  connection
  thread
  (stopped-p t))

(defun make-scheduled (&key (host "localhost") (port 6379))
  (let ((conn (make-connection :host host :port port)))
    (%make-scheduled :connection conn)))

(defun start (scheduled)
  (unless (scheduled-stopped-p scheduled)
    (error "Scheduled thread is already running"))

  (setf (scheduled-stopped-p scheduled) nil)
  (let* ((conn (scheduled-connection scheduled))
         (thread
           (bt:make-thread
            (lambda ()
              (unwind-protect
                   (progn
                     (ensure-connected conn)
                     (loop until (scheduled-stopped-p scheduled)
                           do (handler-case (enqueue-jobs conn (timestamp-to-unix (now)))
                                (error (e)
                                  (vom:error "~A" e)))
                              (sleep (scaled-poll-interval))))
                (with-slots (stopped-p thread connection) scheduled
                  (setf stopped-p t)
                  (setf thread nil)
                  (disconnect connection))))
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
  (when (scheduled-stopped-p scheduled)
    (return-from stop nil))

  (setf (scheduled-stopped-p scheduled) t))

(defun kill (scheduled)
  (with-slots (stopped-p thread connection) scheduled
    (setf stopped-p t)
    (bt:destroy-thread thread)
    (setf thread nil)
    (disconnect connection)))

(defun enqueue-jobs (conn now)
  (with-redis-connection conn
    (loop for payload = (first
                         (red:zrangebyscore (redis-key "retry")
                                            "-inf"
                                            now
                                            :limit '(0 . 1)))
          while payload
          do (red:zrem (redis-key "retry") payload)
             (let* ((job-info (decode-object payload))
                    (queue (or (aget job-info "queue") "default")))
               (enqueue-to-queue conn queue job-info)
               (vom:debug "Enqueued to ~A: ~S" queue job-info)))))
