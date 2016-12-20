(in-package :cl-user)
(defpackage psychiq.launcher
  (:use #:cl
        #:psychiq.specials)
  (:import-from #:psychiq.launcher.manager
                #:make-manager
                #:manager-host
                #:manager-port
                #:manager-queues
                #:manager-children
                #:manager-count)
  (:import-from #:psychiq.launcher.scheduled
                #:scheduled-thread)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:launcher
           #:run
           #:start
           #:stop
           #:kill
           #:wait-for
           #:launcher-status))
(in-package :psychiq.launcher)

(defstruct (launcher (:constructor %make-launcher))
  manager
  scheduled
  heartbeat)

(defun make-launcher (&key (host *default-redis-host*) (port *default-redis-port*) db
                      (concurrency 25) (queue *default-queue-name*)
                      (interval 5))
  (let* ((manager (make-manager :host host
                                :port port
                                :db db
                                :queues (ensure-list queue)
                                :count concurrency
                                :timeout interval))
         (scheduled
           (psychiq.launcher.scheduled:make-scheduled :host host :port port :db db))
         (heartbeat
           (psychiq.launcher.heartbeat:make-heartbeat :host host :port port :db db :manager manager)))
    (%make-launcher :manager manager :scheduled scheduled :heartbeat heartbeat)))

(defmethod print-object ((launcher launcher) stream)
  (print-unreadable-object (launcher stream :type t)
    (let ((manager (launcher-manager launcher)))
      (format stream "REDIS: ~A:~A / CONCURRENCY: ~A / QUEUE: ~A / STATUS: ~A"
              (manager-host manager)
              (manager-port manager)
              (manager-count manager)
              (manager-queues manager)
              (launcher-status launcher)))))

(defun run (&rest initargs
            &key host port db (concurrency 25) queue)
  (declare (ignore host port db concurrency queue))
  (start (apply #'make-launcher initargs)))

(defun wait-for (launcher)
  (psychiq.launcher.manager:wait-for (launcher-manager launcher))
  (let ((thread (scheduled-thread (launcher-scheduled launcher))))
    (when (bt:threadp thread)
      (bt:join-thread thread)))
  t)

(defun start (launcher)
  (psychiq.launcher.manager:start (launcher-manager launcher))
  (psychiq.launcher.scheduled:start (launcher-scheduled launcher))
  (psychiq.launcher.heartbeat:start (launcher-heartbeat launcher))
  launcher)

(defun stop (launcher)
  (psychiq.launcher.manager:stop (launcher-manager launcher))
  (psychiq.launcher.scheduled:stop (launcher-scheduled launcher))
  (psychiq.launcher.heartbeat:stop (launcher-heartbeat launcher))
  t)

(defun kill (launcher)
  (psychiq.launcher.manager:kill (launcher-manager launcher))
  (psychiq.launcher.scheduled:kill (launcher-scheduled launcher))
  (psychiq.launcher.heartbeat:kill (launcher-heartbeat launcher))
  t)

(defun launcher-status (launcher)
  (let ((manager-stopped-p
          (psychiq.launcher.manager:manager-stopped-p (launcher-manager launcher)))
        (scheduled-status
          (psychiq.launcher.scheduled:scheduled-status (launcher-scheduled launcher))))
    (cond
      ((and manager-stopped-p
            (eq scheduled-status :stopped))
       :stopped)
      ((and (not manager-stopped-p)
            (eq scheduled-status :running))
       :running)
      (t
       :stopping))))
