(in-package :cl-user)
(defpackage redqing-test.worker.manager
  (:use #:cl
        #:prove
        #:redqing.worker.manager)
  (:import-from #:redqing.worker.manager
                #:manager-stopped-p)
  (:import-from #:redqing.connection
                #:connect
                #:disconnect))
(in-package :redqing-test.worker.manager)

(plan nil)

(subtest "make-manager"
  (let ((manager (make-manager :queues '("test"))))
    (is-type manager 'manager)
    (ok (manager-stopped-p manager))))

(finalize)
