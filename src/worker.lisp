(in-package :cl-user)
(defpackage redqing.worker
  (:use #:cl)
  (:import-from #:redqing.worker.manager
                #:make-manager)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:run
           #:stop
           #:kill))
(in-package :redqing.worker)

(defun run (queue-or-queues &key (host "localhost") (port 6379) (concurrency 25) (timeout 5))
  (redqing.worker.manager:start
   (make-manager :host host
                 :port port
                 :queues (ensure-list queue-or-queues)
                 :count concurrency)
   :timeout timeout))

(defun stop (worker)
  (redqing.worker.manager:stop worker))

(defun kill (worker)
  (redqing.worker.manager:kill worker))
