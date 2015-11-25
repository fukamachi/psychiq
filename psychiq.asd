#|
  This file is a part of psychiq project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Redis-backed job queueing system

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage psychiq-asd
  (:use :cl :asdf))
(in-package :psychiq-asd)

(defsystem psychiq
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-redis
               :jonathan
               :local-time
               :cl-reexport
               :bordeaux-threads
               :dissect
               :vom
               :alexandria)
  :components ((:module "src"
                :depends-on ("src/specials")
                :components
                ((:file "psychiq" :depends-on ("core" "client"))
                 (:file "client" :depends-on ("core"))
                 (:file "launcher" :depends-on ("core" "launcher-core"))
                 (:module "core"
                  :depends-on ("util")
                  :components
                  ((:file "connection")
                   (:file "worker")
                   (:file "queue" :depends-on ("coder"))
                   (:file "coder")))
                 (:module "launcher-core"
                  :pathname "launcher"
                  :depends-on ("core" "middleware" "util")
                  :components
                  ((:file "processor")
                   (:file "manager" :depends-on ("processor"))
                   (:file "scheduled")))
                 (:module "middleware"
                  :depends-on ("core" "util")
                  :components
                  ((:file "retry-jobs")))
                 (:module "util"
                  :components
                  ((:file "util" :depends-on ("assoc" "redis"))
                   (:file "assoc")
                   (:file "redis")))))
               (:file "src/specials"))
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
  :in-order-to ((test-op (test-op psychiq-test))))
