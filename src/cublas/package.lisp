(defpackage cl-tensor.cublas
  (:use :common-lisp :cffi)
  (:export
    ;; grovel types
    #:+version+
    #:cu-complex
    #:cu-double-complex
    #:handle
    #:status
    #:operation
    #:fill-mode
    #:diag-type
    #:side-mode
    #:pointer-mode
    #:atomics-mode
    #:gemm-algo
    #:math
    #:compute-type
    #:log-callback
    #:cuda-stream

    ;; library
    #:libcublas

    ;; cuBLAS helper functions
    #:create
    #:destroy
    #:get-version
    #:set-property
    #:set-stream
    #:get-stream
    #:get-pointer-mode
    #:set-pointer-mode
    #:set-vector
    #:get-vector
    #:set-matrix
    #:get-matrix
    #:set-vector-async
    #:get-vector-async
    #:set-matrix-async
    #:get-matrix-async
    #:set-atomics-mode
    #:get-atomics-mode
    #:set-math-mode
    #:get-math-mode
    #:logger-configure
    #:get-logger-callback
    #:set-logger-callback

    ;; cuBLAS level-1 functions
    #:isamax #:idamax
    #:isamin #:idamin
    #:saxpy #:daxpy #:caxpy #:zaxpy
    #:scopy #:dcopy #:ccopy #:zcopy
    #:sdot #:ddot #:cdot #:zdot
    #:snrm2 #:dnrm2 #:cnrm2 #:znrm2
    #:srot #:drot #:crot #:zrot
    #:srotg #:drotg #:crotg #:zrotg
    #:srotm #:drotm #:crotm #:zrotm
    #:srotmg #:drotmg #:crotmg #:zrotmg
    #:sscal #:dscal #:cscal #:zscal
    #:sswap #:dswap #:cswap #:zswap

    ;; cuBLAS level-2 functions
    #:sgdmv #:dgdmv #:cgdmv #:zgdmv
    #:sgemv #:dgemv #:cgemv #:zgemv
    #:sger #:dger #:cger #:zger
    #:ssbmv #:dsbmv #:csbmv #:zsbmv
    #:sspmv #:dspmv #:cspmv #:zspmv

    ;; cuBLAS utility
    #:with-handle
    ))
