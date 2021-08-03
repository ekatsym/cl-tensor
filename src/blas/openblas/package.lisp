(defpackage cl-tensor.blas.cblas
  (:nicknames :clt.cblas :clt.cb)
  (:use :common-lisp :cffi)
  (:export
    ;; grovel types and constants
    #:cblas-order
    #:cblas-transpose
    #:cblas-uplo
    #:cblas-diag
    #:cblas-side

    ;; openBLAS level-1 functions
    )
  )
