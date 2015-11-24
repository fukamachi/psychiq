(in-package :cl-user)
(defpackage psychiq.specials
  (:use #:cl)
  (:export #:*psychiq-namespace*
           #:*default-redis-host*
           #:*default-redis-port*
           #:*default-queue-name*
           #:*default-max-retry-attempts*))
(in-package :psychiq.specials)

(defvar *psychiq-namespace* "psychiq")

(defvar *default-redis-host* "localhost")
(defvar *default-redis-port* 6379)

(defvar *default-queue-name* "default")

(defparameter *default-max-retry-attempts* 25)
