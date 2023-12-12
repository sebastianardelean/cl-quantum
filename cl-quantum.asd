(defsystem "cl-quantum"
  :version "0.0.1"
  :author "23ars"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-quantum/tests"))))

(defsystem "cl-quantum/tests"
  :author "23ars"
  :license ""
  :depends-on ("cl-quantum"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-quantum"
  :perform (test-op (op c) (symbol-call :rove :run c)))
