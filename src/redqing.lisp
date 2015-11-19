(in-package :cl-user)
(defpackage redqing
  (:nicknames #:redq)
  (:use #:cl))
(in-package :redqing)

(cl-reexport:reexport-from :redqing.connection
                           :include '(#:*connection*
                                      #:connection
                                      #:connect-toplevel #:disconnect-toplevel
                                      #:connect #:disconnect #:reconnect #:ensure-connected
                                      #:with-connection #:connectedp))

(cl-reexport:reexport-from :redqing.job
                           :include '(#:job #:job-id #:job-options #:additional-options #:perform #:encode-job))

(cl-reexport:reexport-from :redqing.client
                           :include '(#:enqueue #:enqueue-to #:dequeue
                                      #:all-queues #:queue-length #:queue-empty-p #:delete-queue #:slice-queue #:peek-queue))

(cl-reexport:reexport-from :redqing.specials)
