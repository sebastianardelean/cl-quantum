(defpackage cl-quantum
  (:use :cl))
(in-package :cl-quantum)


(defclass qcircuit ()
  (
   (number-of-qubits :accessor number-of-qubits :initarg :number-of-qubits)
   (number-of-bits   :accessor number-of-bits   :initarg :number-of-bits)
   (gates :accessor gates :initarg :gates)
  ))


(defun make-qcircuit (number-of-qubits number-of-bits)
  (make-instance 'qcircuit :number-of-qubits number-of-qubits :number-of-bits number-of-bits :gates '()))


(defconstant HGATE 1)
(defconstant XGATE 2)
(defconstant YGATE 3)
(defconstant ZGATE 4)
(defconstant CNOTGATE 5)
(defconstant INVALID -1)




;;(defmethod (setf gates) (new-val (obj qcircuit))   
;;    (setf (slot-value obj 'gates) new-val))



(defun h-gate (circuit q)
  (let ((x (gates circuit))
        (qubits (number-of-qubits circuit)))
    (if (> qubits q) (setf (gates circuit) (push (list HGATE q -1) x)) (format t "error"))))
        
(defun z-gate (circuit q)
  (let ((x (gates circuit))
        (qubits (number-of-qubits circuit)))
    (if (> qubits q) (setf (gates circuit) (push (list ZGATE q -1) x)) (format t "error"))))

(defun x-gate (circuit q)
  (let ((x (gates circuit))
        (qubits (number-of-qubits circuit)))
    (if (> qubits q) (setf (gates circuit) (push (list XGATE q -1) x)) (format t "error"))))

(defun y-gate (circuit q)
  (let ((x (gates circuit))
        (qubits (number-of-qubits circuit)))
    (if (> qubits q) (setf (gates circuit) (push (list YGATE q -1) x)) (format t "error"))))


(defun cnot-gate (circuit ctrl targ)
  (let ((x (gates circuit))
        (qubits (number-of-qubits circuit)))
    (if (and (> qubits targ) (> qubits ctrl) (/= ctrl targ)) (setf (gates circuit) (push (list CNOTGATE ctrl targ) x)) (format t "error"))))







(defun gate-to-number-map (val)
  (cond
    ((= val 1) "\"hadamard\"")
    ((= val 2) "\"xgate\"")
    ((= val 3) "\"ygate\"")
    ((= val 4) "\"zgate\"")
    ((= val 5) "\"cnotgate\"")))


(defun create-qubits-str (xs &optional result-str)
  (if xs
      (let ((el (car xs)))
        (create-qubits-str (cdr xs) (concatenate 'string result-str (format nil "{\"gate\":~a, \"ctrl\":~a, \"targ\":~a}," (gate-to-number-map (first el)) (second el) (third el)))))
      result-str))

;;;{
;;;   qubits: 3,
;;;   gates: [
;;;         {
;;;          "gate": "hadamard",
;;;          "ctrl":  0,
;;;          "targ": -1,
;;;         },
;;;         {
;;;          "gate": "cnot",
;;;          "ctrl": 0,
;;;          "targ": 1
;;;         }
;;;          ]
;;;


(defun map-to-json (circuit &optional result-string)
  (let (
        (qubits-str (format nil "{\"qubits\":~a, \"bits\":~a, \"gates\":[ " (number-of-qubits circuit) (number-of-bits circuit)))
        (gates-str  (create-qubits-str (gates circuit) "")))
    (concatenate 'string result-string qubits-str (subseq gates-str 0 (1- (length gates-str))) "]}")))



;;;;OpenQASM V3 output

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
   (name     :accessor name     :initarg :name)))

(defun make-qgate (control target name)
  (make-instance 'qgate :controls control :target target :name name))

(defmethod print-object ((obj qgate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "controls ~a, target: ~a, name: ~a" (controls obj) (target obj) (name obj))))


;;;;;QCircuit Class
(defclass qcircuit2 ()
  ((qreg :accessor qreg :initarg :qreg)
   (creg :accessor creg :initarg :creg)
   (gates :accessor gates :initarg :gates)))

(defun make-qcircuit2 (qreg creg)
  (make-instance 'qcircuit2 :qreg qreg :creg creg :gates '()))

(defmethod print-object ((obj qcircuit2) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "qreg: ~a, creg: ~a, gates: ~a" (qreg obj) (creg obj) (gates obj))))

(defun generate-qasm (circuit &optional result-string)
  result-string)
