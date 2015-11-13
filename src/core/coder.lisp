(in-package :cl-user)
(defpackage redqing.coder
  (:use #:cl)
  (:import-from #:jonathan
                #:to-json
                #:parse)
  (:export #:coder
           #:encode
           #:decode

           #:json-coder))
(in-package :redqing.coder)

(defclass coder () ())

(defgeneric encode (coder object)
  (:documentation "Encode a given object, typically an association list."))
(defgeneric decode (coder payload)
  (:documentation "Decode a given payload which was encoded by ENCODE."))


;;
;; JSON coder

(defclass json-coder (coder) ())

(defmethod encode ((coder json-coder) object)
  (declare (ignore coder))
  (jojo:to-json object :from :alist))

(defmethod decode ((coder json-coder) payload)
  (jojo:parse payload :as :alist))
