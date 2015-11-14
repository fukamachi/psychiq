(in-package :cl-user)
(defpackage redqing.worker
  (:use #:cl)
  (:import-from #:redqing.worker.manager
                #:make-manager
                #:manager-children)
  (:import-from #:redqing.worker.processor
                #:processor-thread)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:run
           #:stop
           #:kill
           #:wait-for-processors))
(in-package :redqing.worker)

(defun run (&key (host "localhost") (port 6379) (concurrency 25) (timeout 5)
              (queue "default"))
  (let ((manager (make-manager :host host
                               :port port
                               :queues (ensure-list queue)
                               :count concurrency)))
    (redqing.worker.manager:start manager :timeout timeout)
    manager))

(defun wait-for-processors (manager)
  (map nil #'bt:join-thread
       (mapcar #'processor-thread
               (manager-children manager))))

(defun stop (worker)
  (redqing.worker.manager:stop worker))

(defun kill (worker)
  (redqing.worker.manager:kill worker))
