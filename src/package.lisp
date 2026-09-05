(defpackage #:messagepack-protocol
  (:use #:cl)
  (:nicknames #:stack-messagepack #:stack-msgpack)
  (:export #:messagepack-error
           #:messagepack-encode-error
           #:messagepack-parse-error
           #:messagepack-error-message

           #:msgpack-ext
           #:msgpack-ext-p
           #:msgpack-ext-type
           #:msgpack-ext-data
           #:make-msgpack-ext
           #:msgpack-timestamp
           #:msgpack-timestamp-p
           #:msgpack-timestamp-seconds
           #:msgpack-timestamp-nanoseconds
           #:make-msgpack-timestamp

           #:*messagepack-backend*
           #:messagepack-backend
           #:backend-encode
           #:backend-decode
           #:make-messagepack-backend
           #:use-messagepack-backend

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets

           #:null-p
           #:true-p
           #:false-p

           #:messagepack-serdes-backend
           #:make-messagepack-serdes-backend
           #:use-messagepack-serdes-backend
           #:install-http-messagepack-hooks))

(in-package #:messagepack-protocol)
