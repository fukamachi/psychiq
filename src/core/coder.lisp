(in-package :cl-user)
(defpackage psychiq.coder
  (:use #:cl)
  (:import-from #:jonathan
                #:*false-value*
                #:*null-value*
                #:to-json
                #:parse)
  (:export #:encode-object
           #:decode-object))
(in-package :psychiq.coder)

(defvar *encode-lock*
  (bt:make-lock "encode-object"))

(defun encode-object (object)
  (bt:with-lock-held (*encode-lock*)
    (jojo:to-json object :from :alist)))

(defun decode-object (payload)
  (let ((jojo:*false-value* :false)
        (jojo:*null-value* :null))
    (jojo:parse payload :as :alist)))
