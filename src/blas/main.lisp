(defpackage cl-tensor.blas
  (:nicknames :clt.blas)
  (:use :common-lisp)
  (:export
    ;; Arrays
    #:array #:carray #:cuarray

    ;; Constructors
    #:make-blas-array #:make-blas-array*

    ;; Convertors
    #:coerce-blas-array

    ;; Accessors
    #:blas-array-dimensions
    #:blas-array-dimension
    #:blas-array-rank
    #:blas-array-total-size

    ;; Utilities
    #:trans #:diag #:reshape

    ;; BLAS Level-1
    #:scal #:copy #:axpy #:dot
    #:nrm2 #:asum #:amax #:amin

    ;; BLAS Level-2
    #:gemv #:ger

    ;; BLAS Level-3
    #:gemm))
(in-package :cl-tensor.blas)


;;; Arrays
(defclass carray ()
  ((dimensions :initarg :dimensions :reader carray-dimensions)
   (datum      :initarg :datum      :reader carray-datum)))

(defmethod print-object ((object carray) stream)
  (format stream "<CARRAY :DIMENSIONS ~A :DATUM ~A>"
          (carray-dimensions object)
          (coerce-blas-array 'array object)))

(defclass cuarray ()
  ((dimensions :initarg :dimensions :reader cuarray-dimensions)
   (datum      :initarg :datum      :reader cuarray-datum)))

(defmethod print-object ((object cuarray) stream)
  (format stream "<CUARRAY :DIMENSIONS ~A :DATUM ~A>"
          (cuarray-dimensions object)
          (coerce-blas-array 'array object)) )

;;; Constructors
(defgeneric make-blas-array (array-type-spec dimensions &key element-type
                                                             initial-element
                                                             displaced-to))

(defgeneric make-blas-array* (array-type-spec dimensions &key element-type
                                                              seed next key))

;;; Convertors
(defgeneric coerce-blas-array (array-type-spec blas-array))

;;; Accessors
(defgeneric blas-array-dimensions (blas-array))

(defun blas-array-dimension (blas-array axis-number)
  (nth axis-number (blas-array-dimensions blas-array)))

(defun blas-array-rank (blas-array)
  (length (blas-array-dimensions blas-array)))

(defun blas-array-total-size (blas-array)
  (reduce #'* (blas-array-dimensions blas-array)))

;;; Utilities
(defgeneric trans (a)
  (:documentation
"Return the transposition of A.
A: matrix."))

(defgeneric diag (x)
  (:documentation
"Return diagnal matrix of X.
X: vector."))

(defgeneric reshape (dimensions a)
  (:documentation
"Return DIMENSIONS blas-array that has elements of A.
A: blas-array."))

;; BLAS Level-1
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
"Y <- ALPHA * X + Y.
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

(defgeneric amin (x)
  (:documentation
    "Return argmix_{x_i \in X} |x_i|."))

;; BLAS Level-2
(defgeneric gemv (alpha a x beta y &key trans?)
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

(defgeneric gbmv (alpha a x beta y &key trans? kl ku)
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
