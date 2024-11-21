(defpackage cl-grover/tests/main
  (:use :cl
        :cl-grover
        :rove))
(in-package :cl-grover/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-grover)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
