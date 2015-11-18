(in-package :cl-user)
(defpackage redqing.util
  (:use #:cl)
  (:export #:symbol-name-with-package))
(in-package :redqing.util)

(cl-reexport:reexport-from :redqing.util.assoc)
(cl-reexport:reexport-from :redqing.util.redis)

(defun symbol-name-with-package (symbol)
  (let ((package (symbol-package symbol)))
    (unless package
      (error "Uninterned symbol is not allowed"))
    (format nil "~A::~A"
            (package-name package)
            (symbol-name symbol))))


