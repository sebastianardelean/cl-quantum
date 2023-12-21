(defpackage cl-quantum/example/main
  (:use :cl
        :cl-quantum))
(in-package :cl-quantum/example/main)


(defconstant +QREG+ (make-qregister 2 "q"))
(defconstant +CREG+ (make-cregister 1 "c"))
(defconstant +QC2+   (make-qcircuit +QREG+ +CREG+))

;; Initialize

(defun initialize-circuit ()
  (progn
    (xgate +QC2+ 1)
    (hgate +QC2+ 0)
    (hgate +QC2+ 1)))

;; Oracle f(x) = 0
(defun oracle-f1 ()
  )

;; Oracle f(x) = 1

(defun oracle-f2 ()
  (xgate +QC2+ 1))

;; Oracle f(x) = x
(defun oracle-f3 ()
  (cnotgate +QC2+ 0 1))

;; Oracle f(x) = 1 - x

(defun oracle-f4 ()
  (progn
    (cnotgate +QC2+ 0 1)
    (xgate +QC2+ 1)))

(defun measure-circuit ()
  (progn
    (hgate +QC2+ 0)
    (measure +QC2+ 0 0)))
           
(defun run ()
  (progn
    (initialize-circuit)
    (oracle-f2)
    (measure-circuit)
    (create-openqasm +QC2+ "")))
