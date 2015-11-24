#|
  This file is a part of psychiq project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage psychiq-test-asd
  (:use :cl :asdf))
(in-package :psychiq-test-asd)

(defsystem psychiq-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:psychiq
               :prove)
  :components ((:module "t"
                :components
                ((:module "core"
                  :components
                  ((:test-file "coder")
                   (:test-file "connection")
                   (:test-file "queue")
                   (:test-file "job")))
                 (:module "launcher"
                  :components
                  ((:test-file "processor")
                   (:test-file "manager")
                   (:test-file "scheduled")
                   (:test-file "launcher")))
                 (:module "util"
                  :components
                  ((:test-file "util")
                   (:test-file "assoc"))))))
  :description "Test system for psychiq"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
