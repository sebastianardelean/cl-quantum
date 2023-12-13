(defpackage cl-quantum/tests/main
  (:use :cl
        :cl-quantum
        :rove))
(in-package :cl-quantum/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-quantum)' in your Lisp.

(defconstant +QREG+ (make-qregister 2 "q"))
(defconstant +CREG+ (make-cregister 2 "c"))
(defconstant +QC+ (make-qcircuit qreg creg))
(hgate qc 0)
(xgate qc 1)
(ygate qc 0)
(zgate qc 1)
(measure qc 0 1)

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
