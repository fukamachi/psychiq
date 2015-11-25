(in-package :cl-user)
(defpackage psychiq.util
  (:use #:cl)
  (:export #:symbol-name-with-package))
(in-package :psychiq.util)

(cl-reexport:reexport-from :psychiq.util.assoc)
(cl-reexport:reexport-from :psychiq.util.redis)
(cl-reexport:reexport-from :psychiq.util.concurrency)

(defun symbol-name-with-package (symbol)
  (let ((package (symbol-package symbol)))
    (unless package
      (error "Uninterned symbol is not allowed"))
    (format nil "~A::~A"
            (package-name package)
            (symbol-name symbol))))
