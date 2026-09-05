(in-package #:messagepack-protocol/tests)

(defun %hex (string)
  (let ((clean (remove-if (lambda (c) (find c " \t\n")) string))
        (out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length clean) by 2
          do (vector-push-extend (parse-integer clean :start i :end (+ i 2) :radix 16) out))
    (coerce out '(vector (unsigned-byte 8)))))

(defun %lisp= (a b)
  (cond
    ((and (hash-table-p a) (hash-table-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (loop for k being the hash-keys of a using (hash-value v)
                always (and (nth-value 1 (gethash k b))
                            (%lisp= v (gethash k b))))))
    ((and (vectorp a) (not (stringp a))
          (vectorp b) (not (stringp b)))
     (and (= (length a) (length b))
          (loop for i from 0 below (length a)
                always (%lisp= (aref a i) (aref b i)))))
    ((and (floatp a) (floatp b))
     (< (abs (- a b)) 1d-6))
    ((and (numberp a) (numberp b)) (= a b))
    (t (equal a b))))

(deftest spec-scalars
  (dolist (row '(("c0" :null)
                 ("c2" nil)
                 ("c3" t)
                 ("00" 0)
                 ("01" 1)
                 ("7f" 127)
                 ("cc80" 128)
                 ("e0" -32)
                 ("d0df" -33)
                 ("a0" "")
                 ("a161" "a")
                 ("90" #())
                 ("93010203" #(1 2 3))))
    (destructuring-bind (hex value) row
      (ok (%lisp= value (decode (%hex hex))) hex)
      (ok (equalp (%hex hex) (encode value)) hex)))
  (ok (%lisp= (make-hash-table :test #'equal) (decode (%hex "80"))))
  (ok (equalp (%hex "80") (encode (make-hash-table :test #'equal)))))

(deftest map-and-bin-roundtrip
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "k" ht) 1)
    (ok (%lisp= ht (decode (encode ht)))))
  (let ((raw (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(9 8))))
    (ok (equalp raw (decode (encode raw))))))

(deftest timestamp-roundtrip
  (let ((ts (make-msgpack-timestamp 1)))
    (let ((back (decode (encode ts))))
      (ok (msgpack-timestamp-p back))
      (ok (= 1 (msgpack-timestamp-seconds back)))
      (ok (zerop (msgpack-timestamp-nanoseconds back))))))
