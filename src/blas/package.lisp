(defpackage cl-tensor.blas
  (:nicknames :clt.blas)
  (:use :common-lisp)
  (:export
    ;; BLAS Level-1
    #:scal #:copy #:axpy #:dot
    #:nrm2 #:asum #:amax

    ;; BLAS Level-2
    #:gemv #:gbmv #:ger

    ;; BLAS Level-3
    #:gemm

    ;; extensions
    #:unit #:trans #:diag))
(in-package :cl-tensor.blas)


;;; BLAS Level-1
(defgeneric scal (alpha x)
  (:documentation
"X <- ALPHA * X.
Return X.
ALPHA: scalar.
X: vector."))

(defgeneric copy (x y &key count stride-x stride-y)
  (:documentation
"Y <- X.
Return Y.
X: vector.
Y: vector."))

(defgeneric axpy (alpha x y)
  (:documentation
"Y <- ALPHA * X.
ALPHA: scalar.
X: vector.
Y: vector."))

(defgeneric dot (x y)
  (:documentation
"Return X^T * Y.
X: vector.
Y: vector."))

(defgeneric nrm2 (x)
  (:documentation
"Return X^T * X.
X: vector."))

(defgeneric asum (x)
  (:documentation
"Return sum {|x_i| \in X}."))

(defgeneric amax (x)
  (:documentation
    "Return argmax_{x_i \in X} |x_i|."))

;;; BLAS Level-2
(defgeneric gemv (alpha a x beta y &key trans? diag?)
  (:documentation
"Y <- ALPHA * A * X + BETA * Y.
Y <- ALPHA * A^T * X + BETA * Y.
Y <- ALPHA * A^H * X + BETA * Y.
Return Y.
ALPHA: scalar.
A: matrix.
X: vector.
BETA: scalar.
Y: vector."))

(defgeneric gbmv (alpha a x beta y &key trans? diag? band-width)
  (:documentation
"Y <- ALPHA * A * X + BETA * Y.
Y <- ALPHA * A^T * X + BETA * Y.
Y <- ALPHA * A^H * X + BETA * Y.
Return Y.
ALPHA: scalar.
A: band matrix with BAND-WIDTH.
X: vector.
BETA: scalar.
Y: vector."))

(defgeneric ger (alpha x y a)
  (:documentation
"A <- ALPHA * X * Y^T.
Return A.
ALPHA: scalar.
X: vector.
Y: vector.
A: matrix."))

;;; BLAS Level-3
(defgeneric gemm (alpha a b beta c &key trans-a? trans-b?)
  (:documentation
"C <- ALPHA * op(A) * op(B) + BETA * C.
Return C.
ALPHA: scalar.
A: matrix.
B: matrix.
BETA: scalar.
C: matrix.
op(X) = X, X^T, X^H."))
