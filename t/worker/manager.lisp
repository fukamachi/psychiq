(in-package :cl-user)
(defpackage redqing-test.worker.manager
  (:use #:cl
        #:prove
        #:redqing.worker.manager)
  (:import-from #:redqing.worker.manager
                #:manager-stopped-p)
  (:import-from #:redqing.job
                #:job
                #:perform)
  (:import-from #:redqing.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:redqing.client
                #:enqueue-to)
  (:import-from #:redqing.util.assoc
                #:aget)
  (:import-from #:redqing.util.redis
                #:redis-key))
(in-package :redqing-test.worker.manager)

(plan 4)

(subtest "make-manager"
  (let ((manager (make-manager :queues '("test"))))
    (is-type manager 'manager)
    (ok (manager-stopped-p manager))))

(defparameter *perform-result* nil)
(defclass deferred-job (job) ())

(subtest "normal case"
  (defmethod perform ((job deferred-job) &rest args)
    (declare (ignore args))
    (setf *perform-result* t)
    "OK")
  (let ((conn (connect)))
    (unwind-protect
         ;; Clear
         (with-connection conn
           (red:del (redis-key "queue" "manager-test-normal-case"))
           ;; Enqueue a job
           (enqueue-to "manager-test-normal-case" 'deferred-job))
      (disconnect conn)))
  (setf *perform-result* nil)
  (let ((manager (make-manager :queues '("manager-test-normal-case") :timeout 1)))
    (start manager)
    (sleep 1.2)
    (is *perform-result* t)
    (kill manager)))

(subtest "processor died"
  (defmethod perform ((job deferred-job) &rest args)
    (declare (ignore args))
    (error "Failed"))
  (let ((conn (connect))
        (manager (make-manager :queues '("manager-test-processor-died") :timeout 1))
        job-info)
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "manager-test-processor-died"))
             (red:del (redis-key "retry"))
             ;; Enqueue a job
             (setf job-info
                   (enqueue-to "manager-test-processor-died" 'deferred-job)))

           (start manager)
           (sleep 1.2)
           (with-connection conn
             (let ((payloads
                     (red:zrangebyscore (redis-key "retry")
                                        "-inf"
                                        (+ (local-time:timestamp-to-unix (local-time:now)) 60)
                                        :limit '(0 . 1))))
               (is (length payloads) 1)
               (like (first payloads) (format nil "\"jid\":\"~A\"" (aget job-info "jid")))))
           (stop manager)
           (sleep 1))
      (disconnect conn)
      (kill manager))))

(is (remove-if-not (lambda (thread)
                     (alexandria:starts-with-subseq "redqing " (bt:thread-name thread)))
                   (bt:all-threads))
    nil
    "All threads has been terminated")

(prove:finalize)
