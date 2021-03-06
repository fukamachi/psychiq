(in-package :cl-user)
(defpackage psychiq-test.util.assoc
  (:use #:cl
        #:prove
        #:psychiq.util.assoc))
(in-package :psychiq-test.util.assoc)

(plan 1)

(subtest "aget"
  (let ((data '(("name" . "Eitaro Fukamachi")
                ("email" . "e.arrows@gmail.com"))))
    (is-values (aget data "name")
               '("Eitaro Fukamachi" t))

    (is-values (aget data "age")
               '(nil nil))

    (is (setf (aget data "living") "Japan")
        "Japan")
    (is-values (aget data "living")
               '("Japan" t))
    (is (setf (aget data "living") "Tokyo, Japan")
        "Tokyo, Japan")
    (is-values (aget data "living")
               '("Tokyo, Japan" t))))

(finalize)
