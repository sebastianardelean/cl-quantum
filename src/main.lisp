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
           #:igate
           #:sgate
           #:sdggate
           #:tgate
           #:tdggate
           #:cxgate
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




(defmethod validate-qregister ((qc qcircuit) (reg qregister))
  (let ((qcregs (qregs qc)))
    (if qcregs
        (let ((qreg (find-if (lambda (x) (qregister-equal reg x)) qcregs)))
          (if qreg
              T
              (format t "Register not found in the quantum circuit")))
        (format t "Quantum circuit does not have any quantum registers"))))


(defmethod validate-cregister ((qc qcircuit) (reg cregister))
  (let ((qcregs (cregs qc)))
    (if qcregs
        (let ((creg (find-if (lambda (x) (cregister-equal reg x)) qcregs)))
          (if creg
              T
              (format t "Register not found in the quantum circuit")))
        (format t "Quantum circuit does not have any quantum registers"))))



(defmethod hgate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Hadamard" "h ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))

(defmethod xgate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Pauli-X" "x ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))

(defmethod ygate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Pauli-Y" "y ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))

(defmethod zgate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Pauli-Z" "z ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))


(defmethod igate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Identity" "i ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))


(defmethod sgate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "SQRT(Z)" "s ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))

(defmethod sdggate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Conjugate SQRT(Z)" "sdg ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))


(defmethod tgate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "SQRT(S)" "t ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))

(defmethod tdggate ((qc qcircuit) (reg qregister) position)
  (if (and (> (size reg) position) (validate-qregister qc reg))
      (let ((qg (make-qgate reg position nil -1 "Conjugate SQRT(S)" "tdg ~a[~a];~%" nil)))
        (add-gate qc qg))
      (format t "Qubit position or the register is not valid")))



(defmethod cxgate ((qc qcircuit) (ctrl qregister) ctrlp (targ qregister) targp)
  (if (and (> (size ctrl) ctrlp) (validate-qregister qc ctrl))
      (if (and (> (size targ) targp) (validate-qregister qc targ))
          (let ((qg (make-qgate ctrl ctrlp targ targp "CNOT" "cx ~a[~a], ~a[~a];~%" nil)))
            (add-gate qc qg))
          (format t "Qubit position or the target register is not valid!"))
      (format t "Qubit position or the control register is not valid!")))


(defmethod cygate ((qc qcircuit) (ctrl qregister) ctrlp (targ qregister) targp)
  (if (and (> (size ctrl) ctrlp) (validate-qregister qc ctrl))
      (if (and (> (size targ) targp) (validate-qregister qc targ))
          (let ((qg (make-qgate ctrl ctrlp targ targp "CY" "cy ~a[~a], ~a[~a];~%" nil)))
            (add-gate qc qg))
          (format t "Qubit position or the target register is not valid!"))
      (format t "Qubit position or the control register is not valid!")))



(defmethod czgate ((qc qcircuit) (ctrl qregister) ctrlp (targ qregister) targp)
  (if (and (> (size ctrl) ctrlp) (validate-qregister qc ctrl))
      (if (and (> (size targ) targp) (validate-qregister qc targ))
          (let ((qg (make-qgate ctrl ctrlp targ targp "CZ" "cz ~a[~a], ~a[~a];~%" nil)))
            (add-gate qc qg))
          (format t "Qubit position or the target register is not valid!"))
      (format t "Qubit position or the control register is not valid!")))

(defmethod chgate ((qc qcircuit) (ctrl qregister) ctrlp (targ qregister) targp)
  (if (and (> (size ctrl) ctrlp) (validate-qregister qc ctrl))
      (if (and (> (size targ) targp) (validate-qregister qc targ))
          (let ((qg (make-qgate ctrl ctrlp targ targp "CH" "ch ~a[~a], ~a[~a];~%" nil)))
            (add-gate qc qg))
          (format t "Qubit position or the target register is not valid!"))
      (format t "Qubit position or the control register is not valid!")))

(defmethod measure ((qc qcircuit) (ctrl qregister) ctrlp (targ cregister) targp)
  (if (and (> (size ctrl) ctrlp) (validate-qregister qc ctrl))
      (if (and (> (size targ) targp) (validate-cregister qc targ))
          (let ((qg (make-qgate ctrl ctrlp targ targp "Measurement" "measure ~a[~a] -> ~a[~a];~%" t)))
            (add-gate qc qg))
          (format t "Qubit position or the classical register is not valid!"))
      (format t "Qubit position or the quantum register is not valid!")))




(defun get-gate (gate)
  " Generate the OpenQASM v2.0 quantum gate's instructions.

  Parameters:
  - gate: the quantum gate object.
  "

  (let ((ctrl (ctrlbit gate))
        (targ (targbit gate))
        (gfmt (fmt gate)))
    (if (not (null (ctrlreg gate)))
        (if (not (null (targreg gate)))
            (format nil gfmt (name (ctrlreg gate)) ctrl (name (targreg gate)) targ)
            (format nil gfmt (name (ctrlreg gate)) ctrl))
        (format t "Control register cannot be null"))))
            

(defun get-operators (xs &optional result-str)
  " Generates the list of OpenQASM v2.0 quantum gate's intructions.

  Parameters:
  - xs: the list of quantum gates applied in a quantum circuit.
  - result-str: the string that will contain the OpenQASM v2.0 instructions. By default is empty.
  "
  (if xs
      (let ((el (car xs)))
        (get-operators (cdr xs) (concatenate 'string
                                             result-str
                                             (get-gate el))))
      result-str))





(defun get-qregisters (xs &optional result-str)
  (if xs
      (let ((el (car xs)))
        (get-qregisters (cdr xs) (concatenate 'string
                                              result-str
                                              (format nil "qreg ~a[~a];~%" (name el) (size el))))) result-str))

(defun get-cregisters (xs &optional result-str)
  (if xs
      (let ((el (car xs)))
        (get-cregisters (cdr xs) (concatenate 'string
                                              result-str
                                              (format nil "creg ~a[~a];~%" (name el) (size el))))) result-str))




(defun create-openqasm (qc &optional result-str)
  " Converts the Quantum Circuit into OpenQASM v2.0 code.

  Parameters:
  - qc: the quantum circuit object.
  - result-str: the string that will contain the OpenQASM v2.0 code. By default is empty.
  "
  (let (
        (header (format nil "OPENQASM 2.0;~%include \"qelib1.inc\";~%"))
        (qregs (get-qregisters (qregs qc) ""))
        (cregs (get-cregisters (cregs qc) ""))
        (operators (get-operators (reverse (gates qc)) "")))
    (concatenate 'string result-str header qregs cregs operators)))


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

;; FOR DEBUG only
;;(defvar qr (make-qregister 2 "qr"))
;;(defvar cr (make-cregister 2 "cr"))
;;(defvar qc (make-qcircuit (list qr) (list cr)))
;;(hgate qc qr 0)
;;(xgate qc qr 1)
;;(cxgate qc qr 0 qr 1)
;;(measure qc qr 0 cr 0)
;;(create-openqasm qc "")
