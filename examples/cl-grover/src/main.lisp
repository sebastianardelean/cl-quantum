(defpackage cl-grover
  (:use :cl
        :clq))
(in-package :cl-grover)


(defconstant +QUBITS+ 3)

(defconstant +QREG+ (clq:make-qregister +QUBITS+ "q"))

(defconstant +CREG+ (clq:make-cregister +QUBITS+ "c"))

;; Grover's Algorithm implementation

(defun initialize-s (qc q-index number-of-qubits)
  (if (< q-index number-of-qubits)
      (progn
        (clq:hgate qc +QREG+ q-index)
        (initialize-s qc (+ q-index 1) number-of-qubits)))
  )

;; Oracle for \vert w \rangle = \vert 11 \rangle
(defun oracle (qc)
  (clq:czgate qc +QREG+ 0 +QREG+ 2)
  (clq:czgate qc +QREG+ 1 +QREG+ 2 )
  )

(defun diffuser (qc)
  "Apply H-gate to all qubits"
  (dotimes (q +QUBITS+)
    (clq:hgate qc +QREG+ q))
  (dotimes (q +QUBITS+)
    (clq:xgate qc +QREG+ q))
  (clq:hgate qc +QREG+ 2)
  (clq:ccxgate qc +QREG+ 0 +QREG+ 1 +QREG+ 2)
  (clq:hgate qc +QREG+ 2)
  (dotimes (q +QUBITS+)
    (clq:xgate qc +QREG+ q))
  (dotimes (q +QUBITS+)
    (clq:hgate qc +QREG+ q)))
   




(defun run ()
  (let ((qc   (clq:make-qcircuit (list +QREG+) (list +CREG+))))
    (progn
      (initialize-s qc 0 +QUBITS+)
      (clq:qbarrier qc)
      (oracle qc)
      (clq:qbarrier qc)
      (diffuser qc)
      (clq:qbarrier qc)      
      (clq:measure qc +QREG+ 0 +CREG+ 0)
      (clq:measure qc +QREG+ 1 +CREG+ 1)
      (clq:measure qc +QREG+ 2 +CREG+ 2)
      (clq:save-openqasm-to-file qc "test.openqasm"))))
