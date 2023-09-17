(in-package :cl-user)
(defpackage psychiq-test.launcher.manager
  (:use #:cl
        #:prove
        #:psychiq.launcher.manager)
  (:import-from #:psychiq.launcher.manager
                #:manager-stopped-p)
  (:import-from #:psychiq.worker
                #:worker
                #:perform
                #:worker-queue-name)
  (:import-from #:psychiq.connection
                #:with-connection
                #:connect
                #:disconnect)
  (:import-from #:psychiq.client
                #:enqueue)
  (:import-from #:psychiq.util.assoc
                #:aget)
  (:import-from #:psychiq.util.redis
                #:redis-key)
  (:import-from #:psychiq.util.concurrency
                #:get-value))
(in-package :psychiq-test.launcher.manager)

(plan 4)

(subtest "make-manager"
  (let ((manager (make-manager :queues '("test"))))
    (is-type manager 'manager)
    (ok (manager-stopped-p manager))))

(defparameter *perform-result* nil)
(defclass my-worker (worker) ())

(subtest "normal case"
  (defmethod perform ((worker my-worker) &rest args)
    (declare (ignore args))
    (setf *perform-result* t)
    "OK")
  (defmethod worker-queue-name ((worker my-worker))
    "manager-test-normal-case")
  (let ((conn (connect))
        stats)
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "manager-test-normal-case"))
             (setf stats (psy:stats))
             ;; Enqueue a job
             (enqueue 'my-worker))

           (setf *perform-result* nil)
           (let ((manager (make-manager :queues '("manager-test-normal-case") :timeout 1)))
             (start manager)
             (sleep 1.2)
             (is *perform-result* t)
             (with-connection conn
               (is (- (+ (getf (psy:stats) :processed)
                          (get-value (manager-stat-processed manager)))
                       (getf stats :processed))
                   1)
               (is (- (+ (getf (psy:stats) :failed)
                          (get-value (manager-stat-failed manager)))
                       (getf stats :failed))
                   0))
             (kill manager)))
      (disconnect conn))))

(subtest "processor died"
  (defmethod perform ((worker my-worker) &rest args)
    (declare (ignore args))
    (error "Failed"))
  (defmethod worker-queue-name ((worker my-worker))
    "manager-test-processor-died")
  (let ((conn (connect))
        (manager (make-manager :queues '("manager-test-processor-died") :timeout 1))
        job-info
        stats)
    (unwind-protect
         (progn
           ;; Clear
           (with-connection conn
             (red:del (redis-key "queue" "manager-test-processor-died"))
             (red:del (redis-key "retry"))
             (setf stats (psy:stats))
             ;; Enqueue a job
             (setf job-info
                   (enqueue 'my-worker)))

           (start manager)
           (sleep 1.2)
           (with-connection conn
             (let ((payloads
                     (red:zrangebyscore (redis-key "retry")
                                        "-inf"
                                        (+ (local-time:timestamp-to-unix (local-time:now)) 60)
                                        :limit '(0 . 1))))
               (is (length payloads) 1)
               (let ((failed-info (jojo:parse (first payloads) :as :alist)))
                 (is (aget failed-info "jid") (aget job-info "jid"))
                 (is (aget failed-info "error_class") "COMMON-LISP::SIMPLE-ERROR")
                 (is (aget failed-info "error_message") "Failed")
                 (is (aget failed-info "error_backtrace") nil)
                 (is (aget failed-info "retry_count") 0))))

           (with-connection conn
             (is (- (+ (getf (psy:stats) :processed)
                        (get-value (manager-stat-processed manager)))
                     (getf stats :processed))
                 1)
             (is (- (+ (getf (psy:stats) :failed)
                        (get-value (manager-stat-failed manager)))
                     (getf stats :failed))
                 1))
           (stop manager)
           (sleep 1))
      (disconnect conn)
      (kill manager))))

(sleep 3)
(is (remove-if-not (lambda (thread)
                     (alexandria:starts-with-subseq "psychiq " (bt2:thread-name thread)))
                   (bt2:all-threads))
    nil
    "All threads has been terminated")

(prove:finalize)
