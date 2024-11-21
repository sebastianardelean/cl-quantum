(defpackage clq/tests/main
  (:use :cl
        :clq
        :rove))
(in-package :clq/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clq)' in your Lisp.

(deftest test-create-qreg
  (testing "Creating qregister"
    (let ((reg (make-qregister 2 "q")))
      (ok (= 2 (size reg))))))

(deftest test-create-creg
  (testing "Creating cregister"
    (let ((reg (make-cregister 2 "c")))
      (ok (= 2 (size reg))))))

(deftest test-create-qcircuit
  (testing "Creating circuit"
    (let ((creg (make-cregister 2 "c"))
          (qreg (make-qregister 2 "q"))
          (qc   (make-qcircuit (list qreg) (list creg))))
      (ok (and (= 2 (size (qregs qc))) (= 2 (size (cregs qc))))))))