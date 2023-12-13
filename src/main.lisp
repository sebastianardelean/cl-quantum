;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c) 2023, Sebastian Ardelean (sebastian.ardelean@cs.upt.ro)
;; version 0.0.1 (major minor patch)
;; version history below

#|
DESCRIPTION

CL-QUANTUM (Common Lisp Quantum) is a package, written in
Common Lisp, that allows a user to define a quantum circuit
in terms of quantum register, quantum gates, and classical register
and to export it to OpenQASM. Thus, the circuit defined in Common Lisp
can be simulated or run on IBMQ quantum processors.

CL-QUANTUM was developed to ease the development of quantum algorithms
following the circuit model, while having an easy to use but complex API.
The driving motivation for implementing this package was the need of
having a small library--that can be extended as needed--to define quantum
circuits.

The reason behind chosing Common Lisp was represented by the wish of using
a functional programming language that doesn't have lot of dependencies and
can be ported to different Operating Systems and microprocessor
architectures.

VERSION HISTORY

* Dec 12, 2023 (sebastian ardelean): Finished the implementation on quantum circuits and H, X, Y, Z, and CNOT gate.

* Dec 13, 2023 (sebastian ardelean): Finished the implementation of functions that generate the OpenQASM code; Added documentation.
|#

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants defined for Quantum Gate IDs

(defconstant +HGATE+ 1 "Hadamard gate id used internally.")
(defconstant +XGATE+ 2 "Pauli-X gate id used internally.")
(defconstant +YGATE+ 3 "Pauli-Y gate id used internally.")
(defconstant +ZGATE+ 4 "Pauli-Z gate id used internally.")
(defconstant +CNOTGATE+ 5 "Controlled-Not gate id used internally.")
(defconstant +MEASURE+  30 "Measurement operator id used internally.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Quantum Registers

(defclass qregister ()
  (;; the number of qubits in the register
   (qubits :accessor qubits :initarg :qubits
           :documentation "Quantum register size.")
   ;; the name of the quantum register
   (name :accessor name :initarg :name
         :documentation "Quantum register name.")))

(defun make-qregister (qubits name)
  "Constructor for the Quantum Register.

  Parameters:
  - qubits: the number of qubits in the quantum register.
  - name: quantum register's name.
  "
  (make-instance 'qregister :qubits qubits :name name))

(defmethod print-object ((obj qregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qubits ~a, name: ~a" (qubits obj) (name obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Classical Registers

(defclass cregister ()
  (;; the number of bits in the register
   (bits :accessor bits :initarg :bits
         :documentation "Classical register size.")
   ;; the name of the classical register
   (name :accessor name :initarg :name
         :documentation "Classical register name.")))

(defun make-cregister (bits name)
  "Constructor for the Classical Register.

  Parameters:
  - bits: the number of bits in the classical register.
  - name: classical register's name.
  "
  (make-instance 'cregister :bits bits :name name))

(defmethod print-object ((obj cregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "bits ~a, name: ~a" (bits obj) (name obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Quantum Gate

(defclass qgate ()
  (;; Index of control qubit
   (controls :accessor controls :initarg :controls
             :documentation "Index of the control qubit.")
   ;; Index of target qubit
   (target   :accessor target   :initarg :target
             :documentation "Index of the target qubit.")
   ;; Name of the gate
   (name     :accessor name     :initarg :name
             :documentation "Name of the quantum gate.")
   ;; ID of the gate (see the constants defined above)
   (gateid   :accessor gateid   :initarg :gateid
             :documentation "ID of the quantum gate.")))

(defun make-qgate (control target name id)
  "Constructor for the Quantum Gate.

  Parameters:
  - control: index of the control qubit.
  - target : index of the target qubit.
  - name   : name of the quantum gate.
  - id     : id of the quantum gate.
  "
  (make-instance 'qgate :controls control :target target :name name :gateid id))

(defmethod print-object ((obj qgate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "controls ~a, target: ~a, name: ~a, id: ~a" (controls obj) (target obj) (name obj) (gateid obj))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Quantum Circuit
(defclass qcircuit ()
  (;; Quantum Register
   (qreg :accessor qreg :initarg :qreg
         :documentation "Quantum Register.")
   ;; Classical Register
   (creg :accessor creg :initarg :creg
         :documentation "Classical Register.")
   ;; List of applied quantum gates
   (gates :accessor gates :initarg :gates
          :documentation "List of applied quantum gates.")))

(defun make-qcircuit (qreg creg)
  "Constructor for the Quantum Circuit.

  Parameters:
  - qreg: quantum register.
  - creg: classical register.
  "
  (make-instance 'qcircuit :qreg qreg :creg creg :gates '()))

(defmethod print-object ((obj qcircuit) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qreg: ~a, creg: ~a, gates: ~a" (qreg obj) (creg obj) (gates obj))))


(defmethod hgate ((obj qcircuit) ctrl)
  " Create and apply a Hadamard gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "hadamard" +HGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))
        

(defmethod xgate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-X gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-x" +XGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod ygate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-Y gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-y" +YGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod zgate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-Z gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (> (qubits (qreg obj)) ctrl) 
      (let ((hobj (make-qgate ctrl -1 "pauli-z" +ZGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))

(defmethod cnotgate ((obj qcircuit) ctrl targ)
  " Create and apply a Controlled Not gate.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the control qubit's index in the quantum circuit.
  - targ: the target qubit's index in the quantum circuit.
  "
  (if (and (> (qubits (qreg obj)) ctrl) (> (qubits (qreg obj)) targ) (/= ctrl targ))
      (let ((hobj (make-qgate ctrl targ "cnot" +CNOTGATE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))


(defmethod measure ((obj qcircuit) ctrl targ)
  " Create and apply a Measurement operator.

  Parameters:
  - obj: the quantum circuit on which the operator is applied.
  - ctrl: the qubit's index in the quantum circuit.
  - targ: the bit's index in the classical circuit.
  "
  (if (and (> (qubits (qreg obj)) ctrl) (> (bits (creg obj)) targ) (/= ctrl targ))
      (let ((hobj (make-qgate ctrl targ "measure" +MEASURE+))
            (gate-list (gates obj)))
        (setf (gates obj) (push hobj gate-list))) (format t "error")))



(defun get-gate (gate qregname cregname)
  " Generate the OpenQASM v2.0 quantum gate's instructions.

  Parameters:
  - gate: the quantum gate object.
  - qregname: the name of the quantum register.
  - cregname: the name of the classical register.
  "
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
  " Generates the list of OpenQASM v2.0 quantum gate's intructions.

  Parameters:
  - xs: the list of quantum gates applied in a quantum circuit.
  - qregname: the name of the quantum register.
  - cregname: the name of the classical register.
  - result-str: the string that will contain the OpenQASM v2.0 instructions. By default is empty.
  "
  (if xs
      (let ((el (car xs)))
        (get-operators (cdr xs) qregname cregname (concatenate 'string
                                             result-str
                                             (get-gate el qregname cregname)))) result-str))

(defun create-openqasm (qc &optional result-str)
  " Converts the Quantum Circuit into OpenQASM v2.0 code.

  Parameters:
  - qc: the quantum circuit object.
  - result-str: the string that will contain the OpenQASM v2.0 code. By default is empty.
  "
  (let (
        (header (format nil "OPENQASM 2.0;~%include \"qelib1.inc\";~%"))
        (regs (format nil "qreg ~a[~a];~%creg ~a[~a];~%" (name (qreg qc)) (qubits (qreg qc)) (name (creg qc)) (bits (creg qc))))
        (operators (get-operators (reverse (gates qc)) (name (qreg qc)) (name (creg qc)) "")))
    (concatenate 'string result-str header regs operators)))

(defun create-openqasm-file (qc file-path)
  " Converts the Quantum Circuit into OpenQASM v2.0 code and writes it into a file.

  Parameters:
  - qc: the quantum circuit object.
  - file-path: the file path that will be created.
  "
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (create-openqasm qc ""))))
  


