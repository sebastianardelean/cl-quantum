(defpackage cl-quantum/example/main
  (:use :cl
        :cl-quantum))
(in-package :cl-quantum/example/main)


;; Deutsch-Josza Algrithm Implementation

;; Oracle f(x) = 0
(defun oracle-f1 ()
  )

;; Oracle f(x) = 1

(defun oracle-f2 (qc)
  (xgate qc 1))

;; Oracle f(x) = x
(defun oracle-f3 (qc)
  (cnotgate qc 0 1))

;; Oracle f(x) = 1 - x

(defun oracle-f4 (qc)
  (progn
    (cnotgate qc 0 1)
    (xgate qc 1)))

          
(defun run ()
  (let ((qreg (make-qregister 2 "q"))
        (creg (make-cregister 2 "c"))
        (qc   (make-qcircuit qreg creg)))
    (progn
      (xgate qc 1)
      (hgate qc 0)
      (hgate qc 1)
      (oracle-f2 qc)
      (hgate qc 0)
      (measure qc 0 0)
      (create-openqasm qc ""))))
