(in-package #:messagepack-protocol)

(defclass messagepack-serdes-backend (serdes-protocol:serdes-backend) ())

(defun make-messagepack-serdes-backend ()
  (make-instance 'messagepack-serdes-backend))

(defmethod serdes-protocol:backend-media-type ((backend messagepack-serdes-backend))
  "application/msgpack")

(defmethod serdes-protocol:backend-binary-p ((backend messagepack-serdes-backend))
  t)

(defmethod serdes-protocol:backend-encode ((backend messagepack-serdes-backend) value &key stream)
  (declare (ignore backend))
  (encode value :stream stream))

(defmethod serdes-protocol:backend-decode ((backend messagepack-serdes-backend) source &key)
  (declare (ignore backend))
  (decode source))

(defclass messagepack-binary-input-stream (serdes-protocol:serdes-binary-input-stream)
  ((done :initform nil :accessor msgpack-stream-done-p)))

(defclass messagepack-binary-output-stream (serdes-protocol:serdes-binary-output-stream) ())

(defmethod serdes-protocol:backend-make-input-stream ((backend messagepack-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'messagepack-error :message "messagepack streams are binary"))
  (make-instance 'messagepack-binary-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend messagepack-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'messagepack-error :message "messagepack streams are binary"))
  (make-instance 'messagepack-binary-output-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:stream-decode-value ((stream messagepack-binary-input-stream) &key)
  (if (msgpack-stream-done-p stream)
      :eof
      (let* ((in (serdes-protocol:underlying-stream stream))
             (buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence tmp in)
              do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
              until (< n 4096))
        (when (zerop (length buf))
          (return-from serdes-protocol:stream-decode-value :eof))
        (setf (msgpack-stream-done-p stream) t)
        (decode (coerce buf '(vector (unsigned-byte 8)))))))

(defmethod serdes-protocol:stream-encode-value ((stream messagepack-binary-output-stream) value &key)
  (write-sequence (encode value) (serdes-protocol:underlying-stream stream))
  value)

(defun use-messagepack-serdes-backend ()
  (let ((backend (make-messagepack-serdes-backend)))
    (serdes-protocol:register-format :msgpack backend
                                     :media-type "application/msgpack"
                                     :binary t)
    (serdes-protocol:register-format :messagepack backend
                                     :media-type "application/msgpack"
                                     :binary t)
    backend))

(eval-when (:load-toplevel :execute)
  (use-messagepack-serdes-backend))
