(in-package #:messagepack-protocol)

;;; MessagePack spec. Lisp mapping matches json-protocol:
;;;   nil → false (0xc2)   :null → nil (0xc0)   t → true (0xc3)
;;;   string → str         octet vector → bin
;;;   hash-table / alist → map    vector / list → array
;;;   ext type -1 → msgpack-timestamp; other ext → msgpack-ext

(defstruct (msgpack-ext (:constructor make-msgpack-ext (type data)))
  type data)

(defstruct (msgpack-timestamp (:constructor make-msgpack-timestamp (seconds &optional (nanoseconds 0))))
  seconds nanoseconds)

(defun %u8 (n)
  (make-array n :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(defun %push (buf byte)
  (vector-push-extend byte buf))

(defun %push-be (buf n bytes)
  (loop for shift from (* 8 (1- bytes)) downto 0 by 8
        do (%push buf (ldb (byte 8 shift) n))))

(defun %octet-vector-p (value)
  (and (vectorp value)
       (not (stringp value))
       (let ((et (array-element-type value)))
         (or (equal et '(unsigned-byte 8))
             (and (not (eq et t)) (subtypep et '(unsigned-byte 8)))))))

(defun %alist-p (value)
  (and (consp value)
       (every #'consp value)
       (every (lambda (c) (or (stringp (car c)) (symbolp (car c)))) value)))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (character (string key))))

(defun %write-ext (buf type data)
  (let ((n (length data))
        (type (logand type #xff)))
    (when (> type 127)
      (setf type (- type 256)))
    (let ((utype (if (minusp type) (+ type 256) type)))
      (cond
        ((= n 1) (%push buf #xd4) (%push buf utype) (%push buf (aref data 0)))
        ((= n 2) (%push buf #xd5) (%push buf utype) (loop for b across data do (%push buf b)))
        ((= n 4) (%push buf #xd6) (%push buf utype) (loop for b across data do (%push buf b)))
        ((= n 8) (%push buf #xd7) (%push buf utype) (loop for b across data do (%push buf b)))
        ((= n 16) (%push buf #xd8) (%push buf utype) (loop for b across data do (%push buf b)))
        ((<= n 255)
         (%push buf #xc7) (%push buf n) (%push buf utype)
         (loop for b across data do (%push buf b)))
        ((<= n 65535)
         (%push buf #xc8) (%push-be buf n 2) (%push buf utype)
         (loop for b across data do (%push buf b)))
        (t
         (%push buf #xc9) (%push-be buf n 4) (%push buf utype)
         (loop for b across data do (%push buf b)))))))

(defun %write-timestamp (buf ts)
  (let ((sec (msgpack-timestamp-seconds ts))
        (nsec (msgpack-timestamp-nanoseconds ts)))
    (cond
      ((and (zerop nsec) (<= 0 sec #xffffffff))
       (let ((data (make-array 4 :element-type '(unsigned-byte 8))))
         (loop for i from 0 below 4
               do (setf (aref data i) (ldb (byte 8 (* 8 (- 3 i))) sec)))
         (%write-ext buf -1 data)))
      ((<= sec #x3ffffffff)
       (let ((data (make-array 8 :element-type '(unsigned-byte 8)))
             (packed (logior (ash nsec 34) sec)))
         (loop for i from 0 below 8
               do (setf (aref data i) (ldb (byte 8 (* 8 (- 7 i))) packed)))
         (%write-ext buf -1 data)))
      (t
       (let ((data (make-array 12 :element-type '(unsigned-byte 8))))
         (loop for i from 0 below 4
               do (setf (aref data i) (ldb (byte 8 (* 8 (- 3 i))) nsec)))
         (loop for i from 0 below 8
               do (setf (aref data (+ 4 i)) (ldb (byte 8 (* 8 (- 7 i))) sec)))
         (%write-ext buf -1 data))))))

#+sbcl
(defun %write-float (buf value)
  (if (typep value 'single-float)
      (let ((bits (sb-kernel:single-float-bits value)))
        (%push buf #xca)
        (%push-be buf (logand bits #xffffffff) 4))
      (let ((hi (sb-kernel:double-float-high-bits (float value 1.0d0)))
            (lo (sb-kernel:double-float-low-bits (float value 1.0d0))))
        (%push buf #xcb)
        (%push-be buf (logand hi #xffffffff) 4)
        (%push-be buf lo 4))))

#-sbcl
(defun %write-float (buf value)
  (declare (ignore buf value))
  (error 'messagepack-encode-error :message "MessagePack float encode requires SBCL in 0.1.0"))

(defun encode-value (value buf)
  (cond
    ((eq value :null) (%push buf #xc0))
    ((null value) (%push buf #xc2))
    ((eq value t) (%push buf #xc3))
    ((msgpack-timestamp-p value) (%write-timestamp buf value))
    ((msgpack-ext-p value)
     (%write-ext buf (msgpack-ext-type value) (msgpack-ext-data value)))
    ((integerp value)
     (cond
       ((<= 0 value 127) (%push buf value))
       ((<= -32 value -1) (%push buf (logand value #xff)))
       ((<= 0 value 255) (%push buf #xcc) (%push buf value))
       ((<= 0 value 65535) (%push buf #xcd) (%push-be buf value 2))
       ((<= 0 value #xffffffff) (%push buf #xce) (%push-be buf value 4))
       ((<= 0 value #xffffffffffffffff) (%push buf #xcf) (%push-be buf value 8))
       ((<= -128 value 127) (%push buf #xd0) (%push buf (logand value #xff)))
       ((<= -32768 value 32767) (%push buf #xd1) (%push-be buf (logand value #xffff) 2))
       ((<= (- (expt 2 31)) value (1- (expt 2 31)))
        (%push buf #xd2) (%push-be buf (logand value #xffffffff) 4))
       ((<= (- (expt 2 63)) value (1- (expt 2 63)))
        (%push buf #xd3) (%push-be buf (logand value #xffffffffffffffff) 8))
       (t (error 'messagepack-encode-error :message "integer out of MessagePack range"))))
    ((floatp value) (%write-float buf value))
    ((stringp value)
     (let* ((octets (babel:string-to-octets value :encoding :utf-8))
            (n (length octets)))
       (cond
         ((<= n 31) (%push buf (logior #xa0 n)))
         ((<= n 255) (%push buf #xd9) (%push buf n))
         ((<= n 65535) (%push buf #xda) (%push-be buf n 2))
         (t (%push buf #xdb) (%push-be buf n 4)))
       (loop for b across octets do (%push buf b))))
    ((%octet-vector-p value)
     (let ((n (length value)))
       (cond
         ((<= n 255) (%push buf #xc4) (%push buf n))
         ((<= n 65535) (%push buf #xc5) (%push-be buf n 2))
         (t (%push buf #xc6) (%push-be buf n 4)))
       (loop for b across value do (%push buf b))))
    ((hash-table-p value)
     (let ((n (hash-table-count value)))
       (cond
         ((<= n 15) (%push buf (logior #x80 n)))
         ((<= n 65535) (%push buf #xde) (%push-be buf n 2))
         (t (%push buf #xdf) (%push-be buf n 4)))
       (maphash (lambda (k v)
                  (encode-value (if (or (stringp k) (symbolp k)) (%key-string k) k) buf)
                  (encode-value v buf))
                value)))
    ((%alist-p value)
     (let ((n (length value)))
       (cond
         ((<= n 15) (%push buf (logior #x80 n)))
         ((<= n 65535) (%push buf #xde) (%push-be buf n 2))
         (t (%push buf #xdf) (%push-be buf n 4)))
       (dolist (pair value)
         (encode-value (%key-string (car pair)) buf)
         (encode-value (cdr pair) buf))))
    ((or (vectorp value) (listp value))
     (let* ((seq (if (listp value) (coerce value 'vector) value))
            (n (length seq)))
       (cond
         ((<= n 15) (%push buf (logior #x90 n)))
         ((<= n 65535) (%push buf #xdc) (%push-be buf n 2))
         (t (%push buf #xdd) (%push-be buf n 4)))
       (loop for item across seq do (encode-value item buf))))
    (t
     (error 'messagepack-encode-error
            :message (format nil "cannot encode ~S" (type-of value))))))

(defstruct (%reader (:conc-name %r-) (:constructor %make-reader (octets &optional (index 0))))
  octets index)

(defun %need (reader n)
  (unless (<= (+ (%r-index reader) n) (length (%r-octets reader)))
    (error 'messagepack-parse-error :message "truncated MessagePack")))

(defun %read-byte (reader)
  (%need reader 1)
  (prog1 (aref (%r-octets reader) (%r-index reader))
    (incf (%r-index reader))))

(defun %read-be (reader n)
  (let ((v 0))
    (dotimes (i n v)
      (setf v (logior (ash v 8) (%read-byte reader))))))

(defun %read-bytes (reader n)
  (%need reader n)
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (replace out (%r-octets reader) :start2 (%r-index reader))
    (incf (%r-index reader) n)
    out))

(defun %sign8 (n) (if (> n 127) (- n 256) n))
(defun %sign16 (n) (if (> n 32767) (- n 65536) n))
(defun %sign32 (n) (if (> n #x7fffffff) (- n (ash 1 32)) n))
(defun %sign64 (n) (if (> n #x7fffffffffffffff) (- n (ash 1 64)) n))

#+sbcl
(defun %decode-single (bits)
  (sb-kernel:make-single-float (%sign32 bits)))

#+sbcl
(defun %decode-double (hi lo)
  (sb-kernel:make-double-float (%sign32 hi) lo))

#-sbcl
(defun %decode-single (bits)
  (declare (ignore bits))
  (error 'messagepack-parse-error :message "MessagePack float decode requires SBCL in 0.1.0"))

#-sbcl
(defun %decode-double (hi lo)
  (declare (ignore hi lo))
  (error 'messagepack-parse-error :message "MessagePack float decode requires SBCL in 0.1.0"))

(defun %decode-timestamp (data)
  (ecase (length data)
    (4
     (make-msgpack-timestamp (%read-be (%make-reader data) 4)))
    (8
     (let ((packed (%read-be (%make-reader data) 8)))
       (make-msgpack-timestamp (logand packed #x3ffffffff)
                               (ash packed -34))))
    (12
     (let ((r (%make-reader data)))
       (let ((nsec (%read-be r 4))
             (sec (%read-be r 8)))
         (make-msgpack-timestamp sec nsec))))))

(defun %read-ext (reader n)
  (let* ((type (%sign8 (%read-byte reader)))
         (data (%read-bytes reader n)))
    (if (= type -1)
        (%decode-timestamp data)
        (make-msgpack-ext type data))))

(defun decode-item (reader)
  (let ((b (%read-byte reader)))
    (cond
      ((<= b #x7f) b)
      ((>= b #xe0) (%sign8 b))
      ((<= #x80 b #x8f)
       (let ((ht (make-hash-table :test #'equal)))
         (dotimes (i (logand b #x0f) ht)
           (let ((k (decode-item reader))
                 (v (decode-item reader)))
             (setf (gethash k ht) v)))))
      ((<= #x90 b #x9f)
       (let ((out (make-array (logand b #x0f))))
         (dotimes (i (length out) out)
           (setf (aref out i) (decode-item reader)))))
      ((<= #xa0 b #xbf)
       (babel:octets-to-string (%read-bytes reader (logand b #x1f)) :encoding :utf-8))
      (t
       (ecase b
         (#xc0 :null)
         (#xc2 nil)
         (#xc3 t)
         (#xc4 (let ((n (%read-byte reader))) (%read-bytes reader n)))
         (#xc5 (let ((n (%read-be reader 2))) (%read-bytes reader n)))
         (#xc6 (let ((n (%read-be reader 4))) (%read-bytes reader n)))
         (#xc7 (let ((n (%read-byte reader))) (%read-ext reader n)))
         (#xc8 (let ((n (%read-be reader 2))) (%read-ext reader n)))
         (#xc9 (let ((n (%read-be reader 4))) (%read-ext reader n)))
         (#xca (%decode-single (%read-be reader 4)))
         (#xcb (let ((hi (%read-be reader 4)) (lo (%read-be reader 4)))
                 (%decode-double hi lo)))
         (#xcc (%read-byte reader))
         (#xcd (%read-be reader 2))
         (#xce (%read-be reader 4))
         (#xcf (%read-be reader 8))
         (#xd0 (%sign8 (%read-byte reader)))
         (#xd1 (%sign16 (%read-be reader 2)))
         (#xd2 (%sign32 (%read-be reader 4)))
         (#xd3 (%sign64 (%read-be reader 8)))
         (#xd4 (%read-ext reader 1))
         (#xd5 (%read-ext reader 2))
         (#xd6 (%read-ext reader 4))
         (#xd7 (%read-ext reader 8))
         (#xd8 (%read-ext reader 16))
         (#xd9 (babel:octets-to-string (%read-bytes reader (%read-byte reader)) :encoding :utf-8))
         (#xda (babel:octets-to-string (%read-bytes reader (%read-be reader 2)) :encoding :utf-8))
         (#xdb (babel:octets-to-string (%read-bytes reader (%read-be reader 4)) :encoding :utf-8))
         (#xdc
          (let ((out (make-array (%read-be reader 2))))
            (dotimes (i (length out) out)
              (setf (aref out i) (decode-item reader)))))
         (#xdd
          (let ((out (make-array (%read-be reader 4))))
            (dotimes (i (length out) out)
              (setf (aref out i) (decode-item reader)))))
         (#xde
          (let ((n (%read-be reader 2))
                (ht (make-hash-table :test #'equal)))
            (dotimes (i n ht)
              (setf (gethash (decode-item reader) ht) (decode-item reader)))))
         (#xdf
          (let ((n (%read-be reader 4))
                (ht (make-hash-table :test #'equal)))
            (dotimes (i n ht)
              (setf (gethash (decode-item reader) ht) (decode-item reader))))))))))

(defun encode-messagepack (value)
  (let ((buf (%u8 64)))
    (encode-value value buf)
    (coerce buf '(simple-array (unsigned-byte 8) (*)))))

(defun decode-messagepack (octets)
  (let ((reader (%make-reader octets)))
    (prog1 (decode-item reader)
      (unless (= (%r-index reader) (length octets))
        (error 'messagepack-parse-error :message "trailing MessagePack bytes")))))
