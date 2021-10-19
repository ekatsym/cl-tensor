(defpackage cl-tensor.blas.array
  (:nicknames :clt.blas.array :clt.b.ar)
  (:use
    :common-lisp
    :cl-tensor.core
    :cl-tensor.blas)
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
