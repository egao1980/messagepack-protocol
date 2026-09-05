(in-package #:messagepack-protocol)

(define-condition messagepack-error (error)
  ((message :initarg :message :reader messagepack-error-message :initform nil))
  (:report (lambda (c s)
             (format s "MessagePack error~@[: ~a~]" (messagepack-error-message c)))))

(define-condition messagepack-encode-error (messagepack-error) ())

(define-condition messagepack-parse-error (messagepack-error) ())
