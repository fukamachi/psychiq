(defsystem "psychiq"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("cl-redis"
               "jonathan"
               "local-time"
               "cl-reexport"
               "bordeaux-threads"
               "dissect"
               "vom"
               "uiop"
               "alexandria")
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
                   (:file "scheduled")
                   (:file "heartbeat" :depends-on ("manager" "processor"))))
                 (:module "middleware"
                  :depends-on ("core" "util")
                  :components
                  ((:file "retry-jobs")
                   (:file "logging")))
                 (:module "util"
                  :components
                  ((:file "util" :depends-on ("assoc" "redis" "concurrency"))
                   (:file "assoc")
                   (:file "redis")
                   (:file "concurrency")))))
               (:file "src/specials"))
  :description "Redis-backed job queueing system"
  :in-order-to ((test-op (test-op "psychiq-test"))))
