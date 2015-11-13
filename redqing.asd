#|
  This file is a part of redqing project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Redis-backed job queueing system

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage redqing-asd
  (:use :cl :asdf))
(in-package :redqing-asd)

(defsystem redqing
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-redis
               :jonathan
               :local-time
               :cl-reexport
               :bordeaux-threads
               :vom
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "redqing" :depends-on ("core" "redis" "client"))
                 (:file "client")
                 (:file "redis")
                 (:module "core"
                  :depends-on ("redis")
                  :components
                  ((:file "connection" :depends-on ("coder"))
                   (:file "job")
                   (:file "queue" :depends-on ("connection"))
                   (:file "coder")))
                 (:module "worker"
                  :depends-on ("core" "redis")
                  :components
                  ((:file "processor"))))))
  :description "Redis-backed job queueing system"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op redqing-test))))
