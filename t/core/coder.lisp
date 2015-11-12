(in-package :cl-user)
(defpackage redqing-test.coder
  (:use #:cl
        #:prove
        #:redqing.coder))
(in-package :redqing-test.coder)

(plan 1)

(subtest "json-coder"
  (let ((coder (make-instance 'json-coder)))
    (is-type coder 'coder)
    (let* ((data '(("name" . "Eitaro") ("language" . ("Japanese" "Common Lisp"))))
           (payload (encode coder data)))
      (is payload "{\"name\":\"Eitaro\",\"language\":[\"Japanese\",\"Common Lisp\"]}"
          "Can encode into a JSON string")
      (is (decode coder payload)
          '(("language" "Japanese" "Common Lisp") ("name" . "Eitaro"))
          "Can decode"))))

(finalize)
