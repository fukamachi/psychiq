(in-package :cl-user)
(defpackage psychiq-test.coder
  (:use #:cl
        #:prove
        #:psychiq.coder))
(in-package :psychiq-test.coder)

(plan 1)

(subtest "json-coder"
  (let* ((data '(("name" . "Eitaro") ("language" . ("Japanese" "Common Lisp"))))
         (payload (encode-object data)))
    (is payload "{\"name\":\"Eitaro\",\"language\":[\"Japanese\",\"Common Lisp\"]}"
        "Can encode into a JSON string")
    (is (decode-object payload)
        '(("language" "Japanese" "Common Lisp") ("name" . "Eitaro"))
        "Can decode")))

(finalize)
