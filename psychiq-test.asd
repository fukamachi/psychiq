(defsystem "psychiq-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("psychiq"
               "prove")
  :components ((:module "t"
                :components
                ((:module "core"
                  :components
                  ((:test-file "coder")
                   (:test-file "connection")
                   (:test-file "queue")
                   (:test-file "worker")))
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
  :perform (test-op :after (op c) (symbol-call :prove-asdf :run-test-system c)))
