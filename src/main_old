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
* Dec 21, 2023 (sebastian ardelean): Refactored gate methods and added S,St,T,Tt gates. Removed gate IDs and modified the methods to validate and create a quantum gate.
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
           #:idgate
           #:sgate
           #:sdggate
           #:tgate
           #:tdggate
           #:cnotgate
           #:czgate
           #:cygate
           #:chgate
           #:measure
           #:create-openqasm
           #:create-openqasm-file))

(in-package :cl-quantum)



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

(defgeneric qregister-equal (a b)
  (:documentation "Compare two instances of qregister for equality."))

(defmethod qregister-equal ((a qregister) (b qregister))
  (and (equal (qubits a) (qubits b))
       (equal (name a) (name b))))

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


(defgeneric cregister-equal (a b)
  (:documentation "Compare two instances of cregister for equality."))

(defmethod cregister-equal ((a cregister) (b cregister))
  (and (equal (bits a) (bits b))
       (equal (name a) (name b))))

(defmethod print-object ((obj cregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "bits ~a, name: ~a" (bits obj) (name obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Quantum Gate

(defclass qgate ()
  (
   ;; Control register
   (ctrlregname :accessor ctrlregname :initarg :ctrlregname
                :documentation "Control register")
    ;; Index of control qubit
   (controls :accessor controls :initarg :controls
             :documentation "Index of the control qubit.")
   ;; Target register
   (targregname :accessor targregname :initarg :targregname
                :documentation "Target register")
   ;; Index of target qubit
   (target   :accessor target   :initarg :target
             :documentation "Index of the target qubit.")
   ;; Name of the gate
   (name     :accessor name     :initarg :name
             :documentation "Name of the quantum gate.")
   ;; Format of the generated OpenQASM v2.0 code
   (fmt      :accessor fmt      :initarg :fmt
             :documentation "Format of the generated OpenQASM v2.0 code")
   ;; Is the gate a measurement operator?
   (mop      :accessor mop      :initarg :mop
             :documentation "True only if the gate is a measurement operator")))


(defun make-qgate (ctrlregname control targregname target name qgfmt meas)
  "Constructor for the Quantum Gate.

  Parameters:
  - ctrlregname: control register name.
  - control    : index of the control qubit.
  - targregname: target register name.
  - target     : index of the target qubit.
  - name       : name of the quantum gate.
  - qgfmt      : format of the generate OpenQASM v2.0 code
  - meas       : true if the operator is for measurement
  "
  (make-instance 'qgate :ctrlregname ctrlregname
                        :controls control
                        :targregname targregname 
                        :target target
                        :name name :fmt qgfmt :mop meas))

(defmethod print-object ((obj qgate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ctrlregname: ~a, controls ~a, targregname: ~a, target: ~a, name: ~a, format: ~a, measure: ~a"
            (ctrlregname obj) (controls obj) (targregname obj) (target obj) (name obj) (fmt obj) (mop obj))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition for Quantum Circuit
(defclass qcircuit ()
  (;; List of Quantum Register
   (qreg :accessor qreg :initarg :qreg
         :documentation "Quantum Registers.")
   ;; List of Classical Register
   (creg :accessor creg :initarg :creg
         :documentation "Classical Registers.")
   ;; List of applied quantum gates
   (gates :accessor gates :initarg :gates
          :documentation "List of applied quantum gates.")))

(defun make-qcircuit (qreg creg)
  "Constructor for the Quantum Circuit.

  Parameters:
  - qreg: list of quantum register.
  - creg: list of classical register.
  "
  (make-instance 'qcircuit :qreg qreg :creg creg :gates '()))

(defmethod print-object ((obj qcircuit) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qreg: ~a, creg: ~a, gates: ~a" (qreg obj) (creg obj) (gates obj))))


(defmethod validate-gate-parameters ((obj qcircuit) ctrl targ)
  (if (> (qubits (qreg obj)) ctrl)
      (if (> (qubits (qreg obj)) targ) 
          (if (/= ctrl targ)
              T
              (progn (format t "Target and Control qubits are the same") nil))
          (progn (format t "Target qubit is out of range") nil))
      (progn (format t "Control qubit is out of range") nil)))


(defmethod validate-gate-parameters ((obj qcircuit) (qg qgate))
  (let ((ctrlreg (ctrlregname qg))
        (targreg (targregname qg))
        (ctrlq (controls qg))
        (targq (target qg)))
    (if (> (qubits (qreg obj)) (qubits ctrlreg))
        (if (> (qubits (qreg obj)) (qubits targreg))
            T
            (format t "Target qubit is out of range"))
        (format t "Control qubit is out of range"))))
        
    
        
  

(defmethod validate-measure-parameters ((obj qcircuit) ctrl targ)
  (if (> (qubits (qreg obj)) ctrl)
      (if (> (bits (creg obj)) targ)
          T
          (progn (format t "Target qubit is out of range") nil))
      (progn (format t "Control qubit is out of range") nil)))

(defmethod add-gate ((qc qcircuit) (qg qgate))
  (let ((gate-list (gates qc)))
    (setf (gates qc) (push qg gate-list))))


(defmethod hgate ((obj qcircuit) (ctrlreg qregister) ctrl)
  " Create and apply a Hadamard gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (let ((qg (make-qgate ctrl -1 "hadamard" "h ~a[~a];~%" nil)))
    (if (validate-gate-parameters obj qg)
        (add-gate obj qg)
        (format t "error"))))
        

        

(defmethod xgate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-X gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "pauli-x" "x ~a[~a];~%" nil)
      (format t "error")))

(defmethod ygate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-Y gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "pauli-y" "y ~a[~a];~%" nil)
      (format t "error")))

(defmethod zgate ((obj qcircuit) ctrl)
  " Create and apply a Pauli-Z gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "pauli-y" "z ~a[~a];~%" nil)
      (format t "error")))

(defmethod idgate ((obj qcircuit) ctrl)
  " Create and apply an Identity gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "identity" "id ~a[~a];~%" nil)
      (format t "error")))

(defmethod sgate ((obj qcircuit) ctrl)
  " Create and apply a SQRT(Z) gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "sqrt(Z)" "s ~a[~a];~%" nil)
      (format t "error")))


(defmethod sdggate ((obj qcircuit) ctrl)
  " Create and apply a Conjugate of SQRT(Z) gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "conjugate sqrt(Z)" "sdg ~a[~a];~%" nil)
      (format t "error")))

(defmethod tgate ((obj qcircuit) ctrl)
  " Create and apply a SQRT(S) gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "sqrt(S)" "t ~a[~a];~%" nil)
      (format t "error")))


(defmethod tdggate ((obj qcircuit) ctrl)
  " Create and apply a Conjugate of SQRT(S) gate on qubit ctrl.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the qubit's index in the quantum circuit on which the gate is applied.
  "
  (if (validate-gate-parameters obj ctrl -1)
      (add-gate obj ctrl -1 "conjugate sqrt(S)" "tdg ~a[~a];~%" nil)
      (format t "error")))


(defmethod cnotgate ((obj qcircuit) ctrl targ)
  " Create and apply a Controlled Not gate.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the control qubit's index in the quantum circuit.
  - targ: the target qubit's index in the quantum circuit.
  "
  (if (validate-gate-parameters obj ctrl targ)
      (add-gate obj ctrl targ "cnot" "cx ~a[~a], ~a[~a];~%" nil)
      (format t "error")))


(defmethod czgate ((obj qcircuit) ctrl targ)
  " Create and apply a Controlled-Z gate.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the control qubit's index in the quantum circuit.
  - targ: the target qubit's index in the quantum circuit.
  "
  (if (validate-gate-parameters obj ctrl targ)
      (add-gate obj ctrl targ "cz" "cz ~a[~a], ~a[~a];~%" nil)
      (format t "error")))

(defmethod cygate ((obj qcircuit) ctrl targ)
  " Create and apply a Controlled-Y gate.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the control qubit's index in the quantum circuit.
  - targ: the target qubit's index in the quantum circuit.
  "
  (if (validate-gate-parameters obj ctrl targ)
      (add-gate obj ctrl targ "cy" "cy ~a[~a], ~a[~a];~%" nil)
      (format t "error")))


(defmethod chgate ((obj qcircuit) ctrl targ)
  " Create and apply a Controlled-H gate.

  Parameters:
  - obj: the quantum circuit on which the quantum gate is applied.
  - ctrl: the control qubit's index in the quantum circuit.
  - targ: the target qubit's index in the quantum circuit.
  "
  (if (validate-gate-parameters obj ctrl targ)
      (add-gate obj ctrl targ "ch" "ch ~a[~a], ~a[~a];~%" nil)
      (format t "error")))

(defmethod measure ((obj qcircuit) ctrl targ)
  " Create and apply a Measurement operator.

  Parameters:
  - obj: the quantum circuit on which the operator is applied.
  - ctrl: the qubit's index in the quantum circuit.
  - targ: the bit's index in the classical circuit.
  "
  (if (validate-measure-parameters obj ctrl targ)
      (add-gate obj ctrl targ "measure" "measure ~a[~a] -> ~a[~a];~%" t)
      (format t "error")))



(defun get-gate (gate qregname cregname)
  " Generate the OpenQASM v2.0 quantum gate's instructions.

  Parameters:
  - gate: the quantum gate object.
  - qregname: the name of the quantum register.
  - cregname: the name of the classical register.
  "
  (let ((ctrl (controls gate))
        (targ (target gate))
        (qgfmt (fmt gate))
        (meas (mop gate)))
    (if meas
        (format nil qgfmt qregname ctrl cregname targ)
        (format nil qgfmt qregname ctrl qregname targ))))


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


;; For debug only
;;(defvar qreg (make-qregister 2 "q"))
;;(defvar creg (make-cregister 2 "c"))
;;(defvar qc (make-qcircuit qreg creg))
;;(hgate qc 0)
;;(xgate qc 1)
;;(ygate qc 0)
;;(zgate qc 1)
;;(cnotgate qc 0 1)
;;(sgate qc 0)
;;(sdggate qc 1)
;;(tgate qc 0)
;;(tdggate qc 1)
;;(czgate qc 0 1)
;;(cygate qc 1 0)
;;(chgate qc 0 1)
;;(measure qc 0 1)

