(in-package :cl-user)
(defpackage psychiq.middleware.logging
  (:use #:cl
        #:psychiq.util)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now
                #:format-timestring)
  (:export #:*psychiq-middleware-logging*))
(in-package :psychiq.middleware.logging)

(defparameter *date-format*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\: (:min 2) #\: (:sec 2) :gmt-offset-or-z))

(defparameter *psychiq-middleware-logging*
  (lambda (next)
    (flet ((elapsed (msec)
             (/ (- (get-internal-real-time) msec) 1000.0)))
      (lambda (worker job-info queue)
        (let ((start (get-internal-real-time))
              (context (format nil
                               "~S JID-~A"
                               (class-name (class-of worker))
                               (aget job-info "jid"))))
          (flet ((info (message &rest args)
                   (vom:info "~A ~A: ~A"
                             (format-timestring nil (now) :format *date-format*)
                             context
                             (apply #'format nil message args))))
            (handler-bind ((error
                             (lambda (e)
                               (declare (ignore e))
                               (info "Failed (~A sec)" (elapsed start)))))
              (info "Start")
              (prog1
                  (funcall next worker job-info queue)
                (info "Done (~A sec)" (elapsed start))))))))))
