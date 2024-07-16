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


(defclass qregister ()
  (
   (size :accessor size :initarg :size
         :documentation "Quantum Register size.")
   (name :accessor name :initarg :name
         :documentation "Quantum Register name.")))

(defun make-qregister (size name)
  (make-instance 'qregister :size size :name name))

(defmethod print-object ((obj qregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size ~a, name: ~a" (size obj) (name obj))))



(defmethod qregister-equal ((a qregister) (b qregister))
  (and (equal (size a) (size b))
       (equal (name a) (name b))))

(defclass cregister ()
  (
   (size :accessor size :initarg :size
         :documentation "Classical Register size.")
   
   (name :accessor name :initarg :name
         :documentation "Classical Register name.")))

(defun make-cregister (size name)
  (make-instance 'cregister :size size :name name))

(defmethod cregister-equal ((a cregister) (b cregister))
  (and (equal (size a) (size b))
       (equal (name a) (name b))))

(defmethod print-object ((obj cregister) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size ~a, name: ~a" (size obj) (name obj))))


(defclass qgate()
  (
   (ctrlreg :accessor ctrlreg :initarg :ctrlreg
            :documentation "Control Register.")
   (ctrlbit :accessor ctrlbit :initarg :ctrlbit
            :documentation "Index of the control qubit.")
   (targreg :accessor targreg :initarg :targreg
            :documentation "Target Register.")
   (targbit :accessor targbit :initarg :targbit
            :documentation "Index of the target qubit.")
   (name :accessor name :initarg :name
         :documentation "Quantum gate's name.")
   (fmt :accessor fmt :initarg :fmt
        :documentation "Format of the generated OpenQASM v2.0 code.")
   (mop :accessor mop :initarg :mop
        :documentation "True only if the gate is a measurement operator")))

(defun make-qgate (ctrlreg ctrlbit targreg targbit name fmt mop)
  (make-instance 'qgate :ctrlreg ctrlreg
                        :ctrlbit ctrlbit
                        :targreg targreg
                        :targbit targbit
                        :name name
                        :fmt fmt
                        :mop mop))

(defmethod print-object ((obj qgate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Ctrl register: ~a, Ctrl bits ~a, Target register: ~a, Target bits: ~a, Gate name: ~a, Gate format: ~a, Is Measure: ~a"
            (ctrlreg obj) (ctrlbit obj) (targreg obj) (targbit obj) (name obj) (fmt obj) (mop obj))))

(defclass qcircuit ()
  (
   (qregs :accessor qregs :initarg :qregs
          :documentation "Quantum Registers.")
   (cregs :accessor cregs :initarg :cregs
          :documentation "Classical Registers.")
   (gates :accessor gates :initarg :gates
          :documentation "List of applied quantum gates.")))

(defun make-qcircuit (qregs cregs)
  (make-instance 'qcircuit :qregs qregs
                           :cregs cregs
                           :gates '()))




(defmethod print-object ((obj qcircuit) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qreg: ~a, creg: ~a, gates: ~a" (qregs obj) (cregs obj) (gates obj))))

(defmethod add-gate ((qc qcircuit) (qg qgate))
  (let ((gate-list (gates qc)))
    (setf (gates qc) (push qg gate-list))))




(defmethod validate-gate-register ((qcregs qregister) (reg qregister))
  (if qcreg
      (let (qreg (find-if (lambda (x) (qregister-equal qreg x)) qcregs))
        (if qreg
            (if (> (size qcreg) (size reg))
                T
                (format t "The qubit position is out of range."))
            (format t "Register not found in qcircuit.")))
      (format t "QCircuit list of registers is NIL.")))
              



          
                                     
  (if (> (size (qregs qc)) (size reg)

(defmethod hgate ((qc qcircuit) (reg qregister) position)
  (let ((qg (make-qgate reg position nil -1 "hadamard" "h ~a[~a];~%" nil)))
    (add-gate qc qg)))
