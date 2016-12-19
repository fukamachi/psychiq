(in-package :cl-user)
(defpackage psychiq.connection
  (:use #:cl
        #:psychiq.specials)
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
           #:connect-toplevel
           #:disconnect-toplevel

           ;; Connection APIs
           #:connection
           #:open-connection
           #:close-connection
           #:connectedp))
(in-package :psychiq.connection)

(defvar *connection*)

(defclass connection ()
  ((host :type string
         :initarg :host
         :initform *default-redis-host*)
   (port :type integer
         :initarg :port
         :initform *default-redis-port*)
   (db :type (or integer null)
       :initarg :db
       :initform nil)
   (redis :type (or redis:redis-connection null)
          :initform nil
          :accessor redis-connection)))

(defmethod print-object ((conn connection) stream)
  (print-unreadable-object (conn stream :type t)
    (format stream "~A:~D"
            (slot-value conn 'host)
            (slot-value conn 'port))))

(defgeneric open-connection (conn)
  (:method ((conn connection))
    (with-slots (host port redis db) conn
      (setf redis
            (make-instance 'redis:redis-connection
                           :host host
                           :port port))
      (let ((redis::*connection* redis))
        (when db
          (red:select db))))
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

(defun make-connection (&rest initargs &key host port db)
  (declare (ignore host port db))
  (apply #'make-instance 'connection initargs))

(defun connect (&rest initargs &key host port db)
  (declare (ignore host port db))
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

(defun connect-toplevel (&rest initargs &key host port db)
  (declare (ignore host port db))
  (when (and (boundp '*connection*)
             *connection*)
    (cerror "Disconnect the existing connection"
            "*CONNECTION* is already bound to ~A"
            *connection*)
    (disconnect *connection*))
  (setf *connection* (apply #'connect initargs))
  (setf redis::*connection* (redis-connection *connection*))
  *connection*)

(defun disconnect-toplevel ()
  (when (boundp '*connection*)
    (disconnect *connection*)
    (setf redis::*connection* nil)
    (makunbound '*connection*)))
