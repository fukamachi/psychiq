# Red Qing

[![Build Status](https://travis-ci.org/fukamachi/redqing.svg?branch=master)](https://travis-ci.org/fukamachi/redqing)
[![Coverage Status](https://coveralls.io/repos/fukamachi/redqing/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/redqing)
[![Quicklisp dist](http://quickdocs.org/badge/redqing.svg)](http://quickdocs.org/redqing/)

Red Qing is a Redis-backed job queueing system written in Common Lisp.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

### Writing a job

```common-lisp
(redq:connect-toplevel :host "localhost" :port 6379)

(defclass deferred-job (redq:job) ())
(defmethod redq:perform ((job deferred-job) &rest args)
  ;; blah blah blah
  )
```

### Enqueueing

```common-lisp
;; Enqueueing to "default" queue
(redq:enqueue 'deferred-job '("arg1" "arg2"))

;; Enqueueing to the specific queue
(redq:enqueue-to "myapp-job" 'deferred-job '("arg1" "arg2"))
```

### Starting a worker process

Red Qing provides a [Roswell](https://github.com/snmsts/roswell) script for starting a worker process:

```
$ redqing --host localhost --port 6379 --system myapp-jobs
```

```
$ redqing -h
Usage: redqing [option...]

Options:
    -o, --host HOST           Redis server host
    -p, --port PORT           Redis server port
    -q, --queue QUEUE         Queues to process (Can be specified multiple times)
    -c, --concurrency INT     Processor threads to use
    -s, --system SYSTEM       ASDF system to load before starting
    -h, --help                Show help
```

## Installation

```
$ cd ~/common-lisp
$ git clone https://github.com/fukamachi/redqing
```

```
$ ros install redqing
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
