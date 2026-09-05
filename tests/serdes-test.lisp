(in-package #:messagepack-protocol/tests)

(deftest serdes-messagepack
  (let ((octets (serdes-protocol:encode 7 :format :messagepack)))
    (ok (equalp (encode 7) octets))
    (ok (= 7 (serdes-protocol:decode octets :format :msgpack)))
    (ok (serdes-protocol:format-binary-p :messagepack))
    (ok (string= "application/msgpack" (serdes-protocol:format-media-type :messagepack)))
    (ok (eq :messagepack (serdes-protocol:find-format-for-media-type "application/msgpack")))))
