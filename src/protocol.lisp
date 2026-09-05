(in-package #:messagepack-protocol)

(defvar *messagepack-backend* nil
  "Current MessagePack backend object.")

(defclass messagepack-backend () ())

(defgeneric backend-encode (backend value &key stream))
(defgeneric backend-decode (backend source &key))

(defun null-p (object)
  (eq object :null))

(defun true-p (object)
  (eq object t))

(defun false-p (object)
  (and (null object) (not (eq object :null))))

(defun %source-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (vector
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'messagepack-parse-error :message "MessagePack decode needs octets")))
    (stream
     (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
           (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
       (loop for n = (read-sequence tmp source)
             do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
             until (< n 4096))
       (coerce buf '(vector (unsigned-byte 8)))))))

(defclass native-messagepack-backend (messagepack-backend) ())

(defun make-messagepack-backend ()
  (make-instance 'native-messagepack-backend))

(defmethod backend-encode ((backend native-messagepack-backend) value &key stream)
  (declare (ignore backend))
  (let ((octets (encode-messagepack value)))
    (if stream
        (progn (write-sequence octets stream) (values))
        octets)))

(defmethod backend-decode ((backend native-messagepack-backend) source &key)
  (declare (ignore backend))
  (decode-messagepack (%source-octets source)))

(defun use-messagepack-backend ()
  (setf *messagepack-backend* (make-messagepack-backend)))

(defun encode (value &key stream)
  (unless *messagepack-backend*
    (error 'messagepack-encode-error
           :message "*messagepack-backend* is unbound — load messagepack-protocol"))
  (backend-encode *messagepack-backend* value :stream stream))

(defun decode (source &key)
  (unless *messagepack-backend*
    (error 'messagepack-parse-error
           :message "*messagepack-backend* is unbound — load messagepack-protocol"))
  (backend-decode *messagepack-backend* source))

(defun encode-to-octets (value &key)
  (encode value))

(defun decode-octets (octets &key)
  (decode octets))

(defun install-http-messagepack-hooks ()
  "If http-protocol is loaded, register :messagepack / :msgpack data (de)serializers."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-messagepack-hooks nil))
    (flet ((push-codec (table-sym type fn)
             (let ((s (find-symbol table-sym pkg)))
               (when (and s (boundp s))
                 (setf (symbol-value s)
                       (acons type fn (remove type (symbol-value s) :key #'car)))))))
      (push-codec "*DATA-SERIALIZERS*" :messagepack #'encode)
      (push-codec "*DATA-SERIALIZERS*" :msgpack #'encode)
      (push-codec "*DATA-DESERIALIZERS*" :messagepack #'decode)
      (push-codec "*DATA-DESERIALIZERS*" :msgpack #'decode))
    t))

(eval-when (:load-toplevel :execute)
  (use-messagepack-backend)
  (install-http-messagepack-hooks))
