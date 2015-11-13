(in-package :cl-user)
(defpackage redqing
  (:nicknames #:redq)
  (:use #:cl))
(in-package :redqing)

(cl-reexport:reexport-from :redqing.connection
                           :include '(#:connect #:disconnect #:reconnect #:with-redis-connection #:connectedp))

(cl-reexport:reexport-from :redqing.job
                           :include '(#:job #:perform))

(cl-reexport:reexport-from :redqing.client
                           :include '(#:enqueue))

(cl-reexport:reexport-from :redqing.redis
                           :include '(#:*redqing-namespace*))
