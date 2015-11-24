# Psychiq

[![Build Status](https://travis-ci.org/fukamachi/psychiq.svg?branch=master)](https://travis-ci.org/fukamachi/psychiq)
[![Coverage Status](https://coveralls.io/repos/fukamachi/psychiq/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/psychiq)
[![Quicklisp dist](http://quickdocs.org/badge/psychiq.svg)](http://quickdocs.org/psychiq/)

Psychiq is a Redis-backed job queueing system written in Common Lisp.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

### Writing a job

```common-lisp
(psy:connect-toplevel :host "localhost" :port 6379)

(defclass deferred-job (psy:job) ())
(defmethod psy:perform ((job deferred-job) &rest args)
  ;; blah blah blah
  )
```

### Enqueueing

```common-lisp
;; Enqueueing to "default" queue
(psy:enqueue 'deferred-job '("arg1" "arg2"))

;; Enqueueing to the specific queue
(psy:enqueue-to "myapp-job" 'deferred-job '("arg1" "arg2"))
```

### Starting a worker process

Psychiq provides a [Roswell](https://github.com/snmsts/roswell) script for starting a worker process:

```
$ psychiq --host localhost --port 6379 --system myapp-jobs
```

```
$ psychiq -h
Usage: psychiq [option...]

Options:
    -o, --host HOST           Redis server host
    -p, --port PORT           Redis server port
    -q, --queue QUEUE         Queues to process (Can be specified multiple times)
    -c, --concurrency INT     Processor threads to use
    -s, --system SYSTEM       ASDF system to load before starting
    -h, --help                Show help
```

### Max retry attempts

```common-lisp
(defmethod psy:max-retries ((job-class (eql 'deferred-job)))
  1000)
```

## Installation

```
$ cd ~/common-lisp
$ git clone https://github.com/fukamachi/psychiq
```

```
$ ros install psychiq
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
