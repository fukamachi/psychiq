#|
  This file is a part of redqing project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage redqing-test-asd
  (:use :cl :asdf))
(in-package :redqing-test-asd)

(defsystem redqing-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:redqing
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "redqing")
                 (:module "core"
                  :components
                  ((:test-file "coder")
                   (:test-file "connection")
                   (:test-file "queue")
                   (:test-file "job")))
                 (:module "worker"
                  :components
                  ((:test-file "processor")
                   (:test-file "manager")))
                 (:file "scheduled")
                 (:module "util"
                  :components
                  ((:test-file "util")
                   (:test-file "assoc"))))))
  :description "Test system for redqing"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
