(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage psychiq.util
      (:use #:cl)
      (:export #:symbol-name-with-package
               #:generate-random-id
               #:machine-identity))))
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

(defun generate-random-id (&optional (length 12))
  (format nil "~(~36,6,'0R~)" (random (expt 36 length))))

(defun getpid ()
  #+sbcl (sb-unix:unix-getpid)
  #+ccl (ccl::getpid)
  #+clisp (os:process-id)
  #+ecl (si:getpid)
  #+(or cmu scl) (unix:unix-getpid)
  #+allegro (excl::getpid)
  #+gcl (system:getpid)
  #+lispworks (system::getpid)
  #-(or sbcl ccl clisp ecl cmu scl allegro gcl lispworks)
  (error "getpid is not implemented"))

(defvar *process-nonce* (generate-random-id 6))

(defun machine-identity ()
  (format nil "~A:~A:~A"
          (uiop:hostname)
          (getpid)
          *process-nonce*))
