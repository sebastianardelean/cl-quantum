(defpackage cl-quantum/tests/main
  (:use :cl
        :cl-quantum
        :rove))
(in-package :cl-quantum/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-quantum)' in your Lisp.

(deftest test-create-qreg
  (testing "Creating qregister"
    (let ((reg (make-qregister 2 "q")))
      (ok (= 2 (qubits reg))))))

(deftest test-create-creg
  (testing "Creating cregister"
    (let ((reg (make-cregister 2 "c")))
      (ok (= 2 (bits reg))))))

(deftest test-create-qcircuit
  (testing "Creating circuit"
    (let ((creg (make-cregister 2 "c"))
          (qreg (make-qregister 2 "q"))
          (qc   (make-qcircuit qreg creg)))
      (ok (and (= 2 (qubits (qreg qc))) (= 2 (bits (creg qc))))))))


