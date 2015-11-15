(in-package :cl-user)
(defpackage redqing.coder
  (:use #:cl)
  (:import-from #:jonathan
                #:*false-value*
                #:*null-value*
                #:to-json
                #:parse)
  (:export #:encode-object
           #:decode-object))
(in-package :redqing.coder)

(defun encode-object (object)
  (jojo:to-json object :from :alist))

(defun decode-object (payload)
  (let ((jojo:*false-value* :false)
        (jojo:*null-value* :null))
    (jojo:parse payload :as :alist)))
