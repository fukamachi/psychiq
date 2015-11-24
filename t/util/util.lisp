(in-package :cl-user)
(defpackage psychiq-test.util
  (:use #:cl
        #:prove
        #:psychiq.util))
(in-package :psychiq-test.util)

(plan 2)

(is (symbol-name-with-package 'deferred-job)
    "PSYCHIQ-TEST.UTIL::DEFERRED-JOB")

(is-error (symbol-name-with-package '#:deferred-job)
          'simple-error)

(finalize)
