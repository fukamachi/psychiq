(in-package :cl-user)
(defpackage psychiq.worker
  (:use #:cl
        #:psychiq.specials)
  (:import-from #:psychiq.util
                #:symbol-name-with-package)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:import-from #:alexandria
                #:nconcf
                #:if-let
                #:with-gensyms)
  (:export #:worker
           #:perform
           #:worker-class
           #:worker-retry-count
           #:worker-use-dead-queue-p
           #:worker-queue-name
           #:worker-use-backtrace-p
           #:encode-job
           #:decode-job))
(in-package :psychiq.worker)

(defun generate-random-id (&optional (length 12))
  (format nil "~(~36,8,'0R~)" (random (expt 36 length))))

(defclass worker () ())

(defgeneric perform (worker &rest args)
  (:method ((worker worker) &rest args)
    (declare (ignore args))
    (error "PEFORM is not implemented for ~S" (class-name (class-of worker)))))

(defclass worker-class (standard-class)
  ((retry :initarg :retry
          :initform nil)
   (dead :initarg :dead
         :initform nil)
   (queue :initarg :queue
          :initform nil)
   (backtrace :initarg :backtrace
              :initform nil)))

(defmacro define-worker-class-method (name slot-name &key default)
  (with-gensyms (val class worker)
    `(defgeneric ,name (worker-class)
       (:method ((,worker worker))
         (let ((,class (class-of ,worker)))
           (if (typep ,class 'worker-class)
               (if-let (,val (slot-value ,class ',slot-name))
                 (first ,val)
                 ,default)
               ,default))))))

(define-worker-class-method worker-retry-count retry
  :default *default-max-retry-attempts*)
(define-worker-class-method worker-use-dead-queue-p dead
  :default t)
(define-worker-class-method worker-queue-name queue
  :default *default-queue-name*)
(define-worker-class-method worker-use-backtrace-p backtrace
  :default nil)

(defun contains-class-or-subclasses (class target-classes)
  (flet ((class-inherit-p (target parent)
            (not (null
                  (member parent
                          (c2mop:class-direct-superclasses target)
                          :test #'eq)))))
    (let ((class (if (typep class 'class)
                     class
                     (find-class class))))
      (find-if (lambda (target-class)
                 (let ((target-class (if (typep target-class 'class)
                                         target-class
                                         (find-class target-class nil))))
                   (and target-class
                        (or (eq target-class class)
                            (class-inherit-p target-class class)))))
               target-classes))))

(defmethod initialize-instance :around ((class worker-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'worker direct-superclasses)
    (setf (getf initargs :direct-superclasses)
          (cons (find-class 'worker) direct-superclasses)))
  (apply #'call-next-method class initargs))

(defmethod reinitialize-instance ((class worker-class) &rest initargs)
  (dolist (arg '(:retry :dead :queue :backtrace))
    (unless (getf initargs arg)
      (setf (getf initargs arg) nil)))
  (apply #'call-next-method class initargs))

(defmethod c2mop:ensure-class-using-class :around ((class worker-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'worker direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'worker) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defmethod c2mop:validate-superclass ((class worker-class) (super standard-class))
  t)

(defun encode-job (worker-class args)
  `(("class" . ,(symbol-name-with-package worker-class))
    ("args" . ,args)
    ("jid" . ,(generate-random-id))
    ("created_at" . ,(timestamp-to-unix (now)))))

(defun decode-job (job-info)
  (let ((class (assoc "class" job-info :test #'string=))
        (args  (assoc "args"  job-info :test #'string=))
        (jid   (assoc "jid"   job-info :test #'string=)))
    (unless (and class args jid)
      (error "Invalid job: ~S" job-info))
    (let ((class (read-from-string (cdr class))))
      (check-type class symbol)
      (let ((worker (make-instance class)))
        (check-type worker worker)
        worker))))
