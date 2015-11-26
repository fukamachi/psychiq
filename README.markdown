# Psychiq

[![Build Status](https://travis-ci.org/fukamachi/psychiq.svg?branch=master)](https://travis-ci.org/fukamachi/psychiq)
[![Coverage Status](https://coveralls.io/repos/fukamachi/psychiq/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/psychiq)
[![Quicklisp dist](http://quickdocs.org/badge/psychiq.svg)](http://quickdocs.org/psychiq/)

Psychiq provides background job processing for Common Lisp applications inspired by Ruby's [Sidekiq](http://sidekiq.org).

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Getting Started

### Writing a worker

```common-lisp
(psy:connect-toplevel :host "localhost" :port 6379)

(defclass my-worker (psy:worker) ())
(defmethod psy:perform ((worker my-worker) &rest args)
  ;; Do something
  )
```

### Enqueueing

```common-lisp
;; Enqueueing to "default" queue
(psy:enqueue 'my-worker '("arg1" "arg2"))

;; Enqueueing the job after 300 seconds.
(psy:enqueue-in-sec 300 'my-worker '("arg1" "arg2"))

;; Enqueueing a large number of jobs at a time
;; This is useful if you are pushing tens of thousands of jobs or more
(psy:enqueue-bulk 'my-worker '("arg1" "arg2") '("another" "one") ...)
```

The arguments must be simple JSON datatypes which can be serialized with Jonathan.

### Starting processing

Psychiq provides a [Roswell](https://github.com/snmsts/roswell) script for starting processing:

```
$ psychiq --host localhost --port 6379 --system myapp-workers
```

```
$ psychiq -h
Usage: psychiq [option...]

Options:
    -o, --host HOST                 Redis server host
    -p, --port PORT                 Redis server port
    -q, --queue QUEUE[,WEIGHT]      Queues to process with optional weights (several -q's allowed)
    -c, --concurrency INT           Processor threads to use (default: 25)
    -s, --system SYSTEM             ASDF system to load before starting (several -s's allowed)
    -v, --verbose                   Print more verbose output
    -n, --namespace NAMESPACE       Redis namespace (default: \"psychiq\")
    -V, --version                   Print version
    -h, --help                      Show help
```

## Worker options

### Option 1. Defining a method

```common-lisp
(defclass my-worker () ())

;; Specify max retry attempts. (default: 25)
(defmethod psy:worker-max-retries ((worker my-worker))
  1000)

;; Use a named queue to push. (default: "default")
(defmethod psy:worker-queue-name ((worker my-worker))
  "my-worker-queue")

;; Disable jobs going to the dead job queue. (default: T)
(defmethod psy:worker-use-dead-queue-p ((worker my-worker))
  nil)

;; Whether to save any error backtrace in the retry payload. (default: NIL)
(defmethod psy:worker-use-backtrace-p ((worker my-worker))
  t)
```

### Option 2. Using a metaclass

```common-lisp
(defclass my-worker ()
    ()
  (:metaclass psy:worker-class)
  (:retry 1000)
  (:queue "my-worker-queue")
  (:dead nil)
  (:backtrace t))
```

## Signals

- INT: graceful shutdown, waits for all processors are idle.
- TERM: shutdown immediately

## Error Handling

When getting an error while performing a job, Psychiq will add the job to the "retry" queue. Jobs in the "retry" queue will be retried automatically with an exponential backoff. After 25 attempts, Psychiq move the job to the "dead" queue.

## Requirements

* Redis
* [Roswell](https://github.com/snmsts/roswell) for command-line launcher script.

## Installation

```
$ cd ~/common-lisp
$ git clone https://github.com/fukamachi/psychiq
```

```
$ ros -l ~/common-lisp/psychiq/psychiq.asd install psychiq
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
