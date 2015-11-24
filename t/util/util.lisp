(in-package :cl-user)
(defpackage psychiq-test.util
  (:use #:cl
        #:prove
        #:psychiq.util))
(in-package :psychiq-test.util)

(plan 2)

(is (symbol-name-with-package 'my-worker)
    "PSYCHIQ-TEST.UTIL::MY-WORKER")

(is-error (symbol-name-with-package '#:my-worker)
          'simple-error)

(finalize)
