# Cl-Quantum

## Usage

**cl-quantum** is a package that allows the definition and development of quantum circuits in Common Lisp and to export them to **OpenQASM v2.0**.

The following code presents an example that initializes a quantum circuit made of one 2-qubits quantum register and one 2-bits classical register.
We apply on the circuit Hadamard, Pauli-X, Pauli-Y, Pauli-Z and CNOT gates.

```
  (defvar qreg (make-qregister 2 "q"))
  (defvar creg (make-cregister 2 "c"))
  (defvar qc (make-qcircuit qreg creg))
  (hgate qc 0)
  (xgate qc 1)
  (ygate qc 0)
  (zgate qc 1)
  (cnotgate qc 0 1)
  (measure qc 0 1)

```

To generate the **OpenQASM v2.0** is enough to call the function `(create-openqasm qc "")`, thus generating the string:

```
OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];
 creg c[2];
h q[0];
x q[1];
y q[0];
z q[1];
cx q[0], q[1];
measure q[0] -> c[1]
```

To save the generated code in a file, the function `(defun create-openqasm-file (qc file-path)` must be called.

## Installation

## Author

* Sebastian Ardelean

## Copyright

MIT License

Copyright (c) 2023 Sebastian Ardelean

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

