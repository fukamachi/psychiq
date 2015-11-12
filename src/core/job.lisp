(in-package :cl-user)
(defpackage redqing.job
  (:use #:cl)
  (:import-from #:redqing.connection
                #:connection
                #:connection-coder)
  (:import-from #:redqing.queue
                #:enqueue-to-queue)
  (:import-from #:redqing.coder
                #:encode)
  (:export #:job
           #:job-class
           #:job-args
           #:job-queue

           #:perform
           #:enqueue-job
           #:fail-job))
(in-package :redqing.job)

(defun symbol-name-with-package (symbol)
  (format nil "~A::~A"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defclass job ()
  ((class :type symbol
          :initarg :class
          :initform (error ":class is required")
          :accessor job-class)
   (args :type list
         :initarg :args
         :initform nil
         :accessor job-args)
   (queue :type (or string null)
          :initarg :queue
          :initform nil
          :accessor job-queue)))

(defmethod initialize-instance :after ((job job) &rest initargs)
  (declare (ignore initargs))
  ;; :class must be defined as a class
  (find-class (job-class job) t)

  ;; Set the default queue name
  (unless (job-queue job)
    (setf (job-queue job)
          (symbol-name-with-package (class-name (class-of job))))))

(defmethod print-object ((job job) stream)
  (with-slots (queue class args) job
    (print-unreadable-object (job stream :type job)
      (format stream "queue=~A / class=~S / args=~S"
              queue class args))))

(defgeneric perform (job)
  (:method ((job job))
    ;; nothing to do
    )
  (:method :around ((job job))
    (handler-bind ((error
                     (lambda (exception)
                       (fail-job job exception))))
      (call-next-method))))

(defgeneric enqueue-job (connection job)
  (:method ((conn connection) (job job))
    (let ((payload (encode (connection-coder conn)
                           (job-payload job))))
      (enqueue-to-queue conn
                        (job-queue job)
                        payload))
    job))

(defgeneric fail-job (job exception)
  (:method ((job job) exception)
    (vom:info "~A failed: ~A" job exception)
    ;; TODO sava-failure
    ))

(defgeneric job-payload (job)
  (:method ((job job))
    (with-slots (class args) job
      `(("class" . ,(symbol-name-with-package class))
        ("args" . ,args)))))
