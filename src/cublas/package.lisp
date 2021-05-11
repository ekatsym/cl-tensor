(defpackage cl-tensor.cublas
  (:use :common-lisp :cffi)
  (:export
    ;; grovel types
    #:+cublas-version+
    #:cu-complex
    #:cu-double-complex
    #:cublas-handle
    #:cublas-status
    #:cublas-operation
    #:cublas-fill-mode
    #:cublas-diag-type
    #:cublas-side-mode
    #:cublas-pointer-mode
    #:cublas-atomics-mode
    #:cublas-gemm-algo
    #:cublas-math
    #:cublas-compute-type
    #:cuda-stream

    ;; library
    #:libcublas

    ;; cuBLAS helper functions
    #:cublas-create
    #:cublas-destroy
    #:cublas-get-version

    ;; cuBLAS utility
    #:with-cublas-handle
    ))
