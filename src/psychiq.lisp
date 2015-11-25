(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage psychiq
      (:nicknames #:psy)
      (:use #:cl))))
(in-package :psychiq)

(cl-reexport:reexport-from :psychiq.connection
                           :include '(#:*connection*
                                      #:connection
                                      #:connect-toplevel #:disconnect-toplevel
                                      #:connect #:disconnect #:reconnect #:ensure-connected
                                      #:with-connection #:connectedp))

(cl-reexport:reexport-from :psychiq.worker
                           :include '(#:worker #:perform #:queue-name #:max-retries))

(cl-reexport:reexport-from :psychiq.client
                           :include '(#:enqueue #:enqueue-bulk #:enqueue-in-sec #:dequeue
                                      #:all-queues #:queue-length #:queue-empty-p #:delete-queue #:slice-queue #:peek-queue
                                      #:all-retries #:retry-length
                                      #:all-dead-jobs
                                      #:stats))

(cl-reexport:reexport-from :psychiq.specials)
