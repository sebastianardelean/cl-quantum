(defpackage cl-quantum/tests/main
  (:use :cl
        :cl-quantum
        :rove))
(in-package :cl-quantum/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-quantum)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
