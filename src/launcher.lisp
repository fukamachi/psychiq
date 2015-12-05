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
  scheduled)

(defun make-launcher (&key (host *default-redis-host*) (port *default-redis-port*)
                      (concurrency 25) (queue *default-queue-name*)
                      (interval 5))
  (let ((manager (make-manager :host host
                               :port port
                               :queues (ensure-list queue)
                               :count concurrency
                               :timeout interval))
        (scheduled
          (psychiq.launcher.scheduled:make-scheduled :host host :port port)))
    (%make-launcher :manager manager :scheduled scheduled)))

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
            &key host port (concurrency 25) queue)
  (declare (ignore host port concurrency queue))
  (start (apply #'make-launcher initargs)))

(defun wait-for (launcher)
  (psychiq.launcher.manager:wait-for (launcher-manager launcher))
  (bt:join-thread (scheduled-thread (launcher-scheduled launcher)))
  t)

(defun start (launcher)
  (psychiq.launcher.manager:start (launcher-manager launcher))
  (psychiq.launcher.scheduled:start (launcher-scheduled launcher))
  launcher)

(defun stop (launcher)
  (psychiq.launcher.manager:stop (launcher-manager launcher))
  (psychiq.launcher.scheduled:stop (launcher-scheduled launcher))
  t)

(defun kill (launcher)
  (psychiq.launcher.manager:kill (launcher-manager launcher))
  (psychiq.launcher.scheduled:kill (launcher-scheduled launcher))
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
