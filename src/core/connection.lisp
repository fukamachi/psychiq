(in-package :cl-user)
(defpackage redqing.connection
  (:use #:cl)
  (:import-from #:redqing.coder
                #:json-coder)
  (:import-from #:alexandria
                #:when-let
                #:once-only)
  (:export #:connect
           #:disconnect
           #:reconnect
           #:with-redis-connection

           ;; Connection APIs
           #:connection
           #:open-connection
           #:close-connection
           #:connectedp))
(in-package :redqing.connection)

(defclass connection ()
  ((host :type 'string
         :initarg :host
         :initform "localhost")
   (port :type 'integer
         :initarg :port
         :initform 6379)
   (redis :type (or redis:redis-connection null)
          :initform nil
          :accessor redis-connection)
   (coder :type coder
          :initarg :coder
          :initform (make-instance 'json-coder)
          :accessor connection-coder)))

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
      (return-from close-connection))

    (redis:close-connection (redis-connection conn))
    conn))

(defgeneric connectedp (conn)
  (:method ((conn connection))
    (when-let (redis-conn (redis-connection conn))
      (redis::connection-open-p redis-conn))))

(defmacro with-redis-connection (conn &body body)
  (once-only (conn)
    `(progn
       (unless (connectedp ,conn)
         (open-connection ,conn))
       (let ((redis::*connection* (redis-connection ,conn)))
         ,@body))))

(defun connect (&rest initargs &key host port)
  (declare (ignore host port))
  (open-connection (apply #'make-instance 'connection initargs)))

(defun disconnect (conn)
  (close-connection conn))

(defun reconnect (conn)
  (if (connectedp conn)
      (open-connection (close-connection conn))
      (open-connection conn)))
