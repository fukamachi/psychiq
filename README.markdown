# Psychiq

[![Build Status](https://travis-ci.org/fukamachi/psychiq.svg?branch=master)](https://travis-ci.org/fukamachi/psychiq)
[![Coverage Status](https://coveralls.io/repos/fukamachi/psychiq/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/psychiq)
[![Quicklisp dist](http://quickdocs.org/badge/psychiq.svg)](http://quickdocs.org/psychiq/)

Psychiq provides background job processing for Common Lisp applications inspired by Ruby's [Sidekiq](http://sidekiq.org).

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

### Breaking changes

1. **v0.2.0** - Removed namespaces.  See section below for details.


## Getting Started

### Writing a worker

```common-lisp
(psy:connect-toplevel :host "localhost" :port 6379)

(defclass my-worker (psy:worker) ())
(defmethod psy:perform ((worker my-worker) &rest args)
  ;; Do something
  )
```

The worker class is commonly written in an individual ASDF system because it must be shared with clients and servers.

### Enqueueing (Client)

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

### Starting processing (Server)

Psychiq provides a [Roswell](https://github.com/snmsts/roswell) script for starting processing:

```
$ psychiq.ros --host localhost --port 6379 --system myapp-workers
```

```
$ psychiq.ros -h
Usage: psychiq.ros [option...]

Options:
    -o, --host HOST                 Redis server host (default: localhost)
    -p, --port PORT                 Redis server port (default: 6379)
    -d, --db DBNUM                  Redis server db (default: 0)
    -q, --queue QUEUE[,WEIGHT]      Queues to process with optional weights (several -q's allowed)
    -c, --concurrency INT           Processor threads to use (default: 25)
    -s, --system SYSTEM             ASDF system to load before starting (several -s's allowed)
    -v, --verbose                   Print more verbose output
    -n, --namespace NAMESPACE       Redis namespace (default: "psychiq")
    -V, --version                   Print version
    -h, --help                      Show help
```

## Worker options

```common-lisp
(defclass my-worker (psy:worker) ())

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

## Signals

- INT: graceful shutdown, waits for all processors are idle.
- TERM: shutdown immediately

## Namespaces / Databases

In version 7 Sidekiq [removed namespaces](https://github.com/sidekiq/sidekiq/blob/main/docs/7.0-Upgrade.md#redis-namespace). We handle that by setting `psychiq.specials::*psychiq-namespace*` to `NIL`.  To revert back to the previous behaviour, simply set `*psychiq-namespace*` to `"psychiq"` (the previous default), or whatever namespace you want.

The recommended alternative to namespaces is to use different Redis databases.  This is handled with the `:db` keyword argument to `psy:connect-toplevel`, or the `--db` command line argument for the psychiq.ros script.

## Error Handling

When getting an error while performing a job, Psychiq will add the job to the "retry" queue. Jobs in the "retry" queue will be retried automatically with an exponential backoff. After 25 attempts, Psychiq move the job to the "dead" queue.

## Web UI

Since the data structure which Psychiq stores in Redis is compatible with Ruby's Resque/Sidekiq, Sidekiq's Web UI can be used.

The following assumes you are running Redis on localhost and you only want to access the dashboard from localhost.  This should be the least risky configuration if you are just going to cut/paste.

```ruby
# web.ru
# Sidekiq dashboard
#
# Source: https://github.com/sidekiq/sidekiq/wiki/Monitoring#standalone
#
# Need to do the following:
#   $ gem install rackup sidekiq securerandom rack rack-session
#   $ rackup --host 127.0.0.1 ./web.ru

require 'sidekiq'

Sidekiq.configure_client do |config|
  config.redis = { :size => 1, url: 'redis://127.0.0.1:6379' }
end


require "securerandom"
require "rack/session"
require "sidekiq/web"

# In a multi-process deployment, all Web UI instances should share
# this secret key so they can all decode the encrypted browser cookies
# and provide a working session.
# Rails does this in /config/initializers/secret_token.rb
secret_key = SecureRandom.hex(32)
use Rack::Session::Cookie, secret: secret_key, same_site: true, max_age: 86400
run Sidekiq::Web
```

```
$ gem install rackup sidekiq securerandom rack rack-session
$ rackup --host 127.0.0.1 ./web.ru
```

It will be up at http://127.0.0.1:9292 which allows you to see processes and can retry failed jobs manually.

## Requirements

* SBCL (compiled with sb-thread) or Clozure CL
* Redis
* [Roswell](https://github.com/snmsts/roswell) for command-line launcher script.

## Installation

```
$ ros install psychiq
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015-2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
