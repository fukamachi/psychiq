# Psychiq

[![Build Status](https://travis-ci.org/fukamachi/psychiq.svg?branch=master)](https://travis-ci.org/fukamachi/psychiq)
[![Coverage Status](https://coveralls.io/repos/fukamachi/psychiq/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/psychiq)
[![Quicklisp dist](http://quickdocs.org/badge/psychiq.svg)](http://quickdocs.org/psychiq/)

Psychiq provides background job processing for Common Lisp applications inspired by Ruby's [Sidekiq](http://sidekiq.org).

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Usage

### Writing a worker

```common-lisp
(psy:connect-toplevel :host "localhost" :port 6379)

(defclass my-worker (psy:worker) ())
(defmethod psy:perform ((worker my-worker) &rest args)
  ;; blah blah blah
  )
```

### Enqueueing

```common-lisp
;; Enqueueing to "default" queue
(psy:enqueue 'my-worker '("arg1" "arg2"))

;; Enqueueing to the specific queue
(psy:enqueue-to "myapp-job" 'my-worker '("arg1" "arg2"))
```

### Starting processing

Psychiq provides a [Roswell](https://github.com/snmsts/roswell) script for starting processing:

```
$ psychiq --host localhost --port 6379 --system myapp-workers
```

```
$ psychiq -h
Usage: psychiq [option...]

Options:
    -o, --host HOST           Redis server host
    -p, --port PORT           Redis server port
    -q, --queue QUEUE         Queues to process (several -q's allowed)
    -c, --concurrency INT     Processor threads to use (default: 25)
    -s, --system SYSTEM       ASDF system to load before starting (several -s's allowed)
    -h, --help                Show help
```

### Max retry attempts

```common-lisp
(defmethod psy:max-retries ((worker-class (eql 'my-worker)))
  1000)
```

## Requirements

* Redis
* [Roswell](https://github.com/snmsts/roswell) for command-line launcher script.

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
