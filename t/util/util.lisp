(in-package :cl-user)
(defpackage redqing-test.util
  (:use #:cl
        #:prove
        #:redqing.util))
(in-package :redqing-test.util)

(plan 2)

(is (symbol-name-with-package 'deferred-job)
    "REDQING-TEST.UTIL::DEFERRED-JOB")

(is-error (symbol-name-with-package '#:deferred-job)
          'simple-error)

(finalize)
