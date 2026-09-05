(defsystem "messagepack-protocol"
  :version "0.1.0"
  :description "CLOS MessagePack encode/decode for cl-stack; same Lisp mapping as json-protocol; implements serdes-protocol :messagepack / :msgpack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
:serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "messagepack-protocol/tests"))))

(defsystem "messagepack-protocol/tests"
  :depends-on ("messagepack-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
