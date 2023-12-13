(defpackage cl-quantum
  (:use :cl)
  (:export #:qregister
           #:make-qregister
           #:print-object
           #:qubits
           #:cregister
           #:make-cregister
           #:print-object
           #:bits
           #:qgate
           #:make-qgate
           #:print-object
           #:qcircuit
           #:make-qcircuit
           #:print-object
           #:qreg
           #:creg
           #:hgate
           #:xgate
           #:ygate
           #:zgate
           #:cnotgate
           #:measure
           #:create-openqasm
           #:create-openqasm-file))

(in-package :cl-quantum)



(defconstant +HGATE+ 1)
(defconstant +XGATE+ 2)
(defconstant +YGATE+ 3)
(defconstant +ZGATE+ 4)
(defconstant +CNOTGATE+ 5)
(defconstant +MEASURE+  30)

;;;;OpenQASM V2 output

;;;;;QRegister Class
(defclass qregister ()
  ((qubits :accessor qubits :initarg :qubits)
   (name :accessor name :initarg :name)))

(defun make-qregister (qubits name)
  (make-instance 'qregister :qubits qubits :name name))

(defmethod print-object ((obj qregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qubits ~a, name: ~a" (qubits obj) (name obj))))

;;;;;CRegister Class
(defclass cregister ()
  ((bits :accessor bits :initarg :bits)
   (name :accessor name :initarg :name)))

(defun make-cregister (bits name)
  (make-instance 'cregister :bits bits :name name))

(defmethod print-object ((obj cregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "bits ~a, name: ~a" (bits obj) (name obj))))


;;;;;QGate Class
(defclass qgate ()
  ((controls :accessor controls :initarg :controls)
   (target   :accessor target   :initarg :target)
   (name     :accessor name     :initarg :name)
   (gateid   :accessor gateid   :initarg :gateid)))

(defun make-qgate (control target name id)
  (make-instance 'qgate :controls control :target target :name name :gateid id))

(defmethod print-object ((obj qgate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "controls ~a, target: ~a, name: ~a, id: ~a" (controls obj) (target obj) (name obj) (gateid obj))))



;;;;;QCircuit Class
(defclass qcircuit ()
  ((qreg :accessor qreg :initarg :qreg)
   (creg :accessor creg :initarg :creg)
   (gates :accessor gates :initarg :gates)))

(defun make-qcircuit (qreg creg)
  (make-instance 'qcircuit :qreg qreg :creg creg :gates '()))

(defmethod print-object ((obj qcircuit) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qreg: ~a, creg: ~a, gates: ~a" (qreg obj) (creg obj) (gates obj))))


(defmethod hgate ((obj qcircuit) ctrl)
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "hadamard" +HGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))
        

(defmethod xgate ((obj qcircuit) ctrl)
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-x" +XGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod ygate ((obj qcircuit) ctrl)
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-y" +YGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod zgate ((obj qcircuit) ctrl)
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-z" +ZGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod cnotgate ((obj qcircuit) ctrl targ)
  (if (and (> (qubits (qreg obj)) ctrl) (> (qubits (qreg obj)) targ) (/= ctrl targ))
      (let ((hobj (make-qgate ctrl targ "cnot" +CNOTGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))


(defmethod measure ((obj qcircuit) ctrl targ)
  (if (and (> (qubits (qreg obj)) ctrl) (> (bits (creg obj)) targ) (/= ctrl targ))
      (let ((hobj (make-qgate ctrl targ "measure" +MEASURE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))



(defun get-gate (gate qregname cregname)
  (let ((ctrl (controls gate))
        (targ (target gate))
        (gid  (gateid gate)))
    (cond
      ((= gid 1) (format nil "h ~a[~a];~%" qregname ctrl))
      ((= gid 2) (format nil "x ~a[~a];~%" qregname ctrl))
      ((= gid 3) (format nil "y ~a[~a];~%" qregname ctrl))
      ((= gid 4) (format nil "z ~a[~a];~%" qregname ctrl))
      ((= gid 5) (format nil "cx ~a[~a], ~a[~a];~%" qregname ctrl qregname targ))
      ((= gid 30) (format nil "measure ~a[~a] -> ~a[~a];~%" qregname ctrl cregname targ)))))
    

(defun get-operators (xs qregname cregname &optional result-str)
  (if xs
      (let ((el (car xs)))
        (get-operators (cdr xs) qregname cregname (concatenate 'string
                                             result-str
                                             (get-gate el qregname cregname)))) result-str))

(defun create-openqasm (qc &optional result-str)
  (let (
        (header (format nil "OPENQASM 2.0;~%include \"qelib1.inc\";~%"))
        (regs (format nil "qreg ~a[~a];~% creg ~a[~a];~%" (name (qreg qc)) (qubits (qreg qc)) (name (creg qc)) (bits (creg qc))))
        (operators (get-operators (reverse (gates qc)) (name (qreg qc)) (name (creg qc)) "")))
    (concatenate 'string result-str header regs operators)))

(defun create-openqasm-file (file-path)
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (create-openqasm qc ""))))
  
;;;;Debug Environment preparing
;;(defvar qreg (make-qregister 2 "q"))
;;(defvar creg (make-cregister 2 "c"))
;;(defvar qc (make-qcircuit qreg creg))
;;(hgate qc 0)
;;(xgate qc 1)
;;(ygate qc 0)
;;(zgate qc 1)
;;(measure qc 0 1)





