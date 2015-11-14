# Red Qing

**WIP**

[![Build Status](https://travis-ci.org/fukamachi/redqing.svg?branch=master)](https://travis-ci.org/fukamachi/redqing)
[![Coverage Status](https://coveralls.io/repos/fukamachi/redqing/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/redqing)
[![Quicklisp dist](http://quickdocs.org/badge/redqing.svg)](http://quickdocs.org/redqing/)

Red Qing is a Redis-backed job queueing system written in Common Lisp.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

### Main application

```common-lisp
(defvar *conn* (redq:connect :host "localhost" :port 6379))

(defclass deferred-job (redq:job) ())
(defmethod redq:perform ((job deferred-job) &rest args)
  ;; blah blah blah
  )

(redq:enqueue *conn* 'deferred-job '("arg1" "arg2"))
```

### Worker process

```common-lisp
(redqing.worker:run :host "localhost" :port 6379)
```

## Installation

```
cd ~/common-lisp
git clone https://github.com/fukamachi/redqing
```

```common-lisp
(ql:quickload :redqing)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
