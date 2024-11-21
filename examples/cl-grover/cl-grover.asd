(defsystem "cl-grover"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("cl-quantum")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-grover/tests"))))

(defsystem "cl-grover/tests"
  :author ""
  :license ""
  :depends-on ("cl-grover"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-grover"
  :perform (test-op (op c) (symbol-call :rove :run c)))
