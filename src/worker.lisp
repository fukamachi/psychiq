(in-package :cl-user)
(defpackage redqing.worker
  (:use #:cl)
  (:import-from #:redqing.worker.manager
                #:make-manager
                #:manager-children)
  (:import-from #:redqing.worker.processor
                #:processor-thread)
  (:import-from #:redqing.connection
                #:make-connection)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:run
           #:stop
           #:kill
           #:wait-for-processors))
(in-package :redqing.worker)

(defstruct worker
  manager
  scheduled)

(defun run (&key (host "localhost") (port 6379) (concurrency 25) (timeout 5)
              (queue "default"))
  (let ((manager (make-manager :host host
                               :port port
                               :queues (ensure-list queue)
                               :count concurrency))
        (scheduled
          (redqing.scheduled:make-scheduled :host host :port)))
    (redqing.worker.manager:start manager :timeout timeout)
    (redqing.scheduled:start scheduled)
    (make-worker :manager manager :scheduled scheduled)))

(defun wait-for-processors (worker)
  (map nil #'bt:join-thread
       (mapcar #'processor-thread
               (manager-children (worker-manager worker)))))

(defun stop (worker)
  (redqing.worker.manager:stop (worker-manager worker))
  (redqing.scheduled:stop (worker-scheduled worker)))

(defun kill (worker)
  (redqing.worker.manager:kill (worker-manager worker))
  (redqing.scheduled:kill (worker-scheduled worker)))
