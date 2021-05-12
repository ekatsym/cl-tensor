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
    #:cublas-log-callback
    #:cuda-stream

    ;; library
    #:libcublas

    ;; cuBLAS helper functions
    #:cublas-create
    #:cublas-destroy
    #:cublas-get-version
    #:cublas-set-property
    #:cublas-set-stream
    #:cublas-get-stream
    #:cublas-get-pointer-mode
    #:cublas-set-pointer-mode
    #:cublas-set-vector
    #:cublas-get-vector
    #:cublas-set-matrix
    #:cublas-get-matrix
    #:cublas-set-vector-async
    #:cublas-get-vector-async
    #:cublas-set-matrix-async
    #:cublas-get-matrix-async
    #:cublas-set-atomics-mode
    #:cublas-get-atomics-mode
    #:cublas-set-math-mode
    #:cublas-get-math-mode
    #:cublas-logger-configure
    #:cublas-get-logger-callback
    #:cublas-set-logger-callback

    ;; cuBLAS level-1 functions
    #:cublas-isamax #:cublas-idamax
    #:cublas-isamin #:cublas-idamin
    #:cublas-saxpy #:cublas-daxpy #:cublas-caxpy #:cublas-zaxpy
    #:cublas-scopy #:cublas-dcopy #:cublas-ccopy #:cublas-zcopy
    #:cublas-sdot #:cublas-ddot #:cublas-cdot #:cublas-zdot
    #:cublas-snrm2 #:cublas-dnrm2 #:cublas-cnrm2 #:cublas-znrm2
    #:cublas-srot #:cublas-drot #:cublas-crot #:cublas-zrot
    #:cublas-srotg #:cublas-drotg #:cublas-crotg #:cublas-zrotg
    #:cublas-srotm #:cublas-drotm #:cublas-crotm #:cublas-zrotm
    #:cublas-srotmg #:cublas-drotmg #:cublas-crotmg #:cublas-zrotmg
    #:cublas-sscal #:cublas-dscal #:cublas-cscal #:cublas-zscal
    #:cublas-sswap #:cublas-dswap #:cublas-cswap #:cublas-zswap

    ;; cuBLAS level-2 functions
    #:cublas-sgdmv #:cublas-dgdmv #:cublas-cgdmv #:cublas-zgdmv
    #:cublas-sgemv #:cublas-dgemv #:cublas-cgemv #:cublas-zgemv
    #:cublas-sger #:cublas-dger #:cublas-cger #:cublas-zger
    #:cublas-ssbmv #:cublas-dsbmv #:cublas-csbmv #:cublas-zsbmv
    #:cublas-sspmv #:cublas-dspmv #:cublas-cspmv #:cublas-zspmv

    ;; cuBLAS utility
    #:with-cublas-handle
    ))
