(in-package :cl-user)
(defpackage redqing.connection
  (:use #:cl
        #:redqing.specials)
  (:import-from #:alexandria
                #:when-let
                #:once-only)
  (:export #:*connection*
           #:connect
           #:disconnect
           #:reconnect
           #:ensure-connected
           #:make-connection
           #:with-connection

           ;; Connection APIs
           #:connection
           #:open-connection
           #:close-connection
           #:connectedp))
(in-package :redqing.connection)

(defvar *connection*)

(defclass connection ()
  ((host :type 'string
         :initarg :host
         :initform *default-redis-host*)
   (port :type 'integer
         :initarg :port
         :initform *default-redis-port*)
   (redis :type (or redis:redis-connection null)
          :initform nil
          :accessor redis-connection)))

(defmethod print-object ((conn connection) stream)
  (print-unreadable-object (conn stream :type conn)
    (format stream "~A:~D"
            (slot-value conn 'host)
            (slot-value conn 'port))))

(defgeneric open-connection (conn)
  (:method ((conn connection))
    (with-slots (host port redis) conn
      (setf redis
            (make-instance 'redis:redis-connection
                           :host host
                           :port port)))
    conn))

(defgeneric close-connection (conn)
  (:method ((conn connection))
    (unless (connectedp conn)
      (error "~A is already closed" conn))

    (redis:close-connection (redis-connection conn))
    conn))

(defgeneric connectedp (conn)
  (:method ((conn connection))
    (when-let (redis-conn (redis-connection conn))
      (redis::connection-open-p redis-conn))))

(defmacro with-connection (conn &body body)
  (once-only (conn)
    `(progn
       (ensure-connected ,conn)
       (let ((*connection* ,conn)
             (redis::*connection* (redis-connection ,conn)))
         ,@body))))

(defun make-connection (&rest initargs &key host port)
  (declare (ignore host port))
  (apply #'make-instance 'connection initargs))

(defun connect (&rest initargs &key host port)
  (declare (ignore host port))
  (open-connection (apply #'make-connection initargs)))

(defun ensure-connected (conn)
  (if (connectedp conn)
      conn
      (open-connection conn)))

(defun disconnect (conn)
  (when (connectedp conn)
    (close-connection conn)))

(defun reconnect (conn)
  (if (connectedp conn)
      (open-connection (close-connection conn))
      (open-connection conn)))
