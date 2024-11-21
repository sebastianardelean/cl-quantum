(defsystem "clq"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "clq/tests"))))

(defsystem "clq/tests"
  :author ""
  :license ""
  :depends-on ("clq"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clq"
  :perform (test-op (op c) (symbol-call :rove :run c)))
