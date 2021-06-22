(defpackage cl-tensor.blas
  (:nicknames :clt.blas)
  (:use :common-lisp)
  (:export
    ;; Level 1
    #:amax #:amin #:asum #:axpy #:copy #:copy* #:dot #:nrm2 #:scal

    ;; Level 2
    #:sbmv #:gemv #:ger

    ;; Level 3
    #:gemm
    )
  )
