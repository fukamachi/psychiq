(in-package :cl-user)
(defpackage redqing.worker
  (:use #:cl
        #:redqing.specials)
  (:import-from #:redqing.worker.manager
                #:make-manager
                #:manager-host
                #:manager-port
                #:manager-queues
                #:manager-children)
  (:import-from #:redqing.worker.processor
                #:processor-thread)
  (:import-from #:redqing.connection
                #:make-connection)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:worker
           #:run
           #:start
           #:stop
           #:kill
           #:worker-status
           #:wait-for-processors))
(in-package :redqing.worker)

(defstruct (worker (:constructor %make-worker))
  manager
  scheduled)

(defun make-worker (&key (host *default-redis-host*) (port *default-redis-port*)
                      (concurrency 25) (queue *default-queue-name*)
                      (interval 5))
  (let ((manager (make-manager :host host
                               :port port
                               :queues (ensure-list queue)
                               :count concurrency
                               :timeout interval))
        (scheduled
          (redqing.worker.scheduled:make-scheduled :host host :port port)))
    (%make-worker :manager manager :scheduled scheduled)))

(defmethod print-object ((worker worker) stream)
  (print-unreadable-object (worker stream :type worker)
    (let ((manager (worker-manager worker)))
      (format stream "REDIS: ~A:~A / PROCESSORS: ~A / QUEUE: ~A / STATUS: ~A"
              (manager-host manager)
              (manager-port manager)
              (length (manager-children manager))
              (manager-queues manager)
              (worker-status worker)))))

(defun run (&rest initargs
            &key host port (concurrency 25) queue)
  (declare (ignore host port concurrency queue))
  (start (apply #'make-worker initargs)))

(defun wait-for-processors (worker)
  (map nil #'bt:join-thread
       (mapcar #'processor-thread
               (manager-children (worker-manager worker)))))

(defun start (worker)
  (redqing.worker.manager:start (worker-manager worker))
  (redqing.worker.scheduled:start (worker-scheduled worker))
  worker)

(defun stop (worker)
  (redqing.worker.manager:stop (worker-manager worker))
  (redqing.worker.scheduled:stop (worker-scheduled worker)))

(defun kill (worker)
  (redqing.worker.manager:kill (worker-manager worker) nil)
  (redqing.worker.manager:wait-manager-ends (worker-manager worker))
  (redqing.worker.scheduled:kill (worker-scheduled worker) t))

(defun worker-status (worker)
  (let ((manager-stopped-p
          (redqing.worker.manager:manager-stopped-p (worker-manager worker)))
        (scheduled-status
          (redqing.worker.scheduled:scheduled-status (worker-scheduled worker))))
    (cond
      ((and manager-stopped-p
            (eq scheduled-status :stopped))
       :stopped)
      ((and (not manager-stopped-p)
            (eq scheduled-status :running))
       :running)
      (t
       :stopping))))
